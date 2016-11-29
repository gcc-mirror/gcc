#!/usr/bin/env python3

# This script is used by maintainers to modify Bugzilla entries in batch
# mode.
# Currently it can remove and add a release from/to PRs that are prefixed
# with '[x Regression]'. Apart from that, it can also change target
# milestones and optionally enhance the list of known-to-fail versions.
#
# The script utilizes the Bugzilla API, as documented here:
# http://bugzilla.readthedocs.io/en/latest/api/index.html
#
# It requires the simplejson, requests, semantic_version packages.
# In case of openSUSE:
#   zypper in python3-simplejson python3-requests
#   pip3 install semantic_version
#
# Sample usages of the script:
#
# $ ./maintainer-scripts/branch_changer.py api_key --new-target-milestone=6.2:6.3 --comment '6.2 has been released....' --add-known-to-fail=6.2 --limit 3
#
# The invocation will set target milestone to 6.3 for all issues that
# have mistone equal to 6.2. Apart from that, a comment is added to these
# issues and 6.2 version is added to known-to-fail versions.
# At maximum 3 issues will be modified and the script will run
# in dry mode (no issues are modified), unless you append --doit option.
#
# $ ./maintainer-scripts/branch_changer.py api_key --new-target-milestone=5.5:6.3 --comment 'GCC 5 branch is being closed' --remove 5 --limit 3
#
# Very similar to previous invocation, but instead of adding to known-to-fail,
# '5' release is removed from all issues that have the regression prefix.
#
# $ ./maintainer-scripts/branch_changer.py api_key --add=7:8
#
# Aforementioned invocation adds '8' release to the regression prefix of all
# issues that contain '7' in its regression prefix.
#

import requests
import json
import argparse
import re

from semantic_version import Version

base_url = 'https://gcc.gnu.org/bugzilla/rest.cgi/'
statuses = ['UNCONFIRMED', 'ASSIGNED', 'SUSPENDED', 'NEW', 'WAITING', 'REOPENED']
search_summary = ' Regression]'
regex = '(.*\[)([0-9\./]*)( [rR]egression])(.*)'

class Bug:
    def __init__(self, data):
        self.data = data
        self.versions = None
        self.fail_versions = []
        self.is_regression = False

        self.parse_summary()
        self.parse_known_to_fail()

    def parse_summary(self):
        m = re.match(regex, self.data['summary'])
        if m != None:
            self.versions = m.group(2).split('/')
            self.is_regression = True
            self.regex_match = m

    def parse_known_to_fail(self):
        v = self.data['cf_known_to_fail'].strip()
        if v != '':
            self.fail_versions = [x for x in re.split(' |,', v) if x != '']

    def name(self):
        return 'PR%d (%s)' % (self.data['id'], self.data['summary'])

    def remove_release(self, release):
        # Do not remove last value of [x Regression]
        if len(self.versions) == 1:
            return
        self.versions = list(filter(lambda x: x != release, self.versions))

    def add_release(self, releases):
        parts = releases.split(':')
        assert len(parts) == 2
        for i, v in enumerate(self.versions):
            if v == parts[0]:
                self.versions.insert(i + 1, parts[1])
                break

    def add_known_to_fail(self, release):
        if release in self.fail_versions:
            return False
        else:
            self.fail_versions.append(release)
            return True

    def update_summary(self, api_key, doit):
        summary = self.data['summary']
        new_summary = self.serialize_summary()
        if new_summary != summary:
            print(self.name())
            print('  changing summary: "%s" to "%s"' % (summary, new_summary))
            self.modify_bug(api_key, {'summary': new_summary}, doit)

            return True

        return False

    def change_milestone(self, api_key, old_milestone, new_milestone, comment, new_fail_version, doit):
        old_major = Bug.get_major_version(old_milestone)
        new_major = Bug.get_major_version(new_milestone)

        print(self.name())
        args = {}
        if old_major == new_major:
            args['target_milestone'] = new_milestone
            print('  changing target milestone: "%s" to "%s" (same branch)' % (old_milestone, new_milestone))
        elif self.is_regression and new_major in self.versions:
            args['target_milestone'] = new_milestone
            print('  changing target milestone: "%s" to "%s" (regresses with the new milestone)' % (old_milestone, new_milestone))
        else:
            print('  not changing target milestone: not a regression or does not regress with the new milestone')

        if 'target_milestone' in args and comment != None:
            print('  adding comment: "%s"' % comment)
            args['comment'] = {'comment': comment }

        if new_fail_version != None:
            if self.add_known_to_fail(new_fail_version):
                s = self.serialize_known_to_fail()
                print('  changing known_to_fail: "%s" to "%s"' % (self.data['cf_known_to_fail'], s))
                args['cf_known_to_fail'] = s

        if len(args.keys()) != 0:
            self.modify_bug(api_key, args, doit)
            return True
        else:
            return False

    def serialize_summary(self):
        assert self.versions != None
        assert self.is_regression == True

        new_version = '/'.join(self.versions)
        new_summary = self.regex_match.group(1) + new_version + self.regex_match.group(3) + self.regex_match.group(4)
        return new_summary

    def serialize_known_to_fail(self):
        assert type(self.fail_versions) is list
        return ', '.join(sorted(self.fail_versions, key = lambda x: Version(x, partial = True)))

    def modify_bug(self, api_key, params, doit):
        u = base_url + 'bug/' + str(self.data['id'])

        data = {
            'ids': [self.data['id']],
            'api_key': api_key }

        data.update(params)

        if doit:
            r = requests.put(u, data = json.dumps(data), headers = {"content-type": "text/javascript"})
            print(r)

    @staticmethod
    def get_major_version(release):
        parts = release.split('.')
        assert len(parts) == 2 or len(parts) == 3
        return '.'.join(parts[:-1])

    @staticmethod
    def get_bugs(api_key, query):
        u = base_url + 'bug'
        r = requests.get(u, params = query)
        return [Bug(x) for x in r.json()['bugs']]

def search(api_key, remove, add, limit, doit):
    bugs = Bug.get_bugs(api_key, {'api_key': api_key, 'summary': search_summary, 'bug_status': statuses})
    bugs = list(filter(lambda x: x.is_regression, bugs))

    modified = 0
    for bug in bugs:
        if remove != None:
            bug.remove_release(remove)
        if add != None:
            bug.add_release(add)

        if bug.update_summary(api_key, doit):
            modified += 1
            if modified == limit:
                break

    print('\nModified PRs: %d' % modified)

def replace_milestone(api_key, limit, old_milestone, new_milestone, comment, add_known_to_fail, doit):
    bugs = Bug.get_bugs(api_key, {'api_key': api_key, 'bug_status': statuses, 'target_milestone': old_milestone})

    modified = 0
    for bug in bugs:
        if bug.change_milestone(api_key, old_milestone, new_milestone, comment, add_known_to_fail, doit):
            modified += 1
            if modified == limit:
                break

    print('\nModified PRs: %d' % modified)

parser = argparse.ArgumentParser(description='')
parser.add_argument('api_key', help = 'API key')
parser.add_argument('--remove', nargs = '?', help = 'Remove a release from summary')
parser.add_argument('--add', nargs = '?', help = 'Add a new release to summary, e.g. 6:7 will add 7 where 6 is included')
parser.add_argument('--limit', nargs = '?', help = 'Limit number of bugs affected by the script')
parser.add_argument('--doit', action = 'store_true', help = 'Really modify BUGs in the bugzilla')
parser.add_argument('--new-target-milestone', help = 'Set a new target milestone, e.g. 4.9.3:4.9.4 will set milestone to 4.9.4 for all PRs having milestone set to 4.9.3')
parser.add_argument('--add-known-to-fail', help = 'Set a new known to fail for all PRs affected by --new-target-milestone')
parser.add_argument('--comment', help = 'Comment a PR for which we set a new target milestore')

args = parser.parse_args()
# Python3 does not have sys.maxint
args.limit = int(args.limit) if args.limit != None else 10**10

if args.remove != None or args.add != None:
    search(args.api_key, args.remove, args.add, args.limit, args.doit)
if args.new_target_milestone != None:
    t = args.new_target_milestone.split(':')
    assert len(t) == 2
    replace_milestone(args.api_key, args.limit, t[0], t[1], args.comment, args.add_known_to_fail, args.doit)
