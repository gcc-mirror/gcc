#!/usr/bin/perl -w
# Create the timezone tables  for java/util/TimeZone from the 
# standard timezone sources by Arthur David Olson (as used by glibc)
# 
# This needs the files from the package tzdata2000h which may be found
# at ftp://ftp.cs.mu.oz.au/pub/.

$TIMEZONEDIR = "tzdata";
@TIMEZONEFILES = ("africa", "antarctica", "asia", "australasia",
		  "europe", "northamerica", "pacificnew", "southamerica",
		  "../tzabbrevs");

# rules hash table:
#  key is a rule name
#  value is either "-" (no daylight savings) or a list of three elements:
#     $value[0] = end savings rule  (list containing MONTH, DAY and TIME)
#     $value[1] = start savings rule (ditto)
#     $value[2] = daylight offset in milliseconds
my %rules = ("-" => "-");

# timezones list, list of pairs:
#  $timezones[$i][0] is a timezone name
#  $timezones[$i][1] = raw offset in milliseconds
#  $timezones[$i][2] = rule in the same format as the value of 
#                      the rules table, but TIME in milliseconds
#  $timezones[$i][3] = list of timezone names with this rule (aliases)
my @timezones = ( [ "GMT", 0, "-", [ "GMT", "UTC" ] ]);


# parse the offset of form +/-hh:mm:ss  (:ss is optional) and return it
# in milliseconds against UTC
sub parseOffset($) {
    my $offset = $_[0];
    $offset =~ /^([+-]?)(\d+)(:(\d+)(:(\d+))?)?$/
	or die "Can't parse offset $offset";
    my $seconds = $2 * 3600;
    $seconds += $4 * 60 if ($3);
    $seconds += $6 if ($3 && $5);
    if ($1 eq "-") {
        $seconds = - $seconds;
    }
    return $seconds * 1000;
}

# parse the time of form +/-hh:mm:ss[swguz]  (:ss is optional) and return it
# in milliseconds since midnight in local wall time
    my $timezonename;
sub parseTime($$$) {
    my ($rawoffset, $stdoffset, $time) = @_;
    $time =~ /^([+-]?)(\d+):(\d+)(:(\d+))?([swguz]?)$/
	or die "Can't parse time $time";
    my ($hour, $min) = ($2, $3);
    my $sec = ($4) ? $5 : 0;
    my $millis =  ((($hour * 60) + $min) * 60 + $sec) * 1000;
    if ($1 eq "-") {
      $millis = -$millis;
    }
    # Normally millis is in wall time, adjust for utc and standard time.
    if ($6 =~ /[guz]/) {
	$millis += $rawoffset + $stdoffset;
    } elsif ($6 =~ /s/) {
	$millis += $stdoffset;
    }
    return $millis;
}

my %monthnames = 
    ( "Jan" => "1",
      "Feb" => "2",
      "Mar" => "3",
      "Apr" => "4",
      "May" => "5",
      "Jun" => "6",
      "Jul" => "7",
      "Aug" => "8",
      "Sep" => "9",
      "Oct" => "10",
      "Nov" => "11",
      "Dec" => "12" );
sub parseMonth($) {
    my $month = $monthnames{"$_[0]"} or die "Unknown month $_[0]";
    return $month;
}

my %weekdaynames = 
    ( "Sun" => "7",
      "Mon" => "1",
      "Tue" => "2",
      "Wed" => "3",
      "Thu" => "4",
      "Fri" => "5",
      "Sat" => "6" );
sub parseWeekday($) {
    my $weekday = $weekdaynames{"$_[0]"} or die "Unknown weekday $_[0]";
    return $weekday;
}

my @weekdayjavanames = 
    ( "Calendar.SUNDAY",
      "Calendar.MONDAY",
      "Calendar.TUESDAY",
      "Calendar.WEDNESDAY",
      "Calendar.THURSDAY",
      "Calendar.FRIDAY",
      "Calendar.SATURDAY" );
my @daysInMonths = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );
sub parseDay($$$) {
    my ($dayoffset, $month, $day) = @_;
    if ($day =~ /^\d+$/) {
	return "$day, 0";
    } elsif ($day =~ /^last([A-Z][a-z][a-z])$/) {
	my $weekday = ( parseWeekday($1) + $dayoffset + 7 ) % 7;
	if ($dayoffset) {
	    my $day = $daysInMonths[$month - 1] + $dayoffset;
	    warn "Can only approximate $day with dayoffset in $file" 
		if ($month == 2);
	    return "$day, -$weekdayjavanames[$weekday]";
	} else {
	    return "-1, $weekdayjavanames[$weekday]";
	}
    } elsif ($day =~ /^([A-Z][a-z][a-z])>=(\d+)$/) {
	my $start = $2 + $dayoffset;
	my $weekday = ( parseWeekday($1) + $dayoffset + 7 ) % 7;
	if (($start % 7) == 1) {
	    $start = ($start + 6) / 7;
	    return "$start, $weekdayjavanames[$weekday]";
	} else {
	    return "$start, -$weekdayjavanames[$weekday]";
	}
    } else {
	die "Unknown day $day";
    }
}

my @monthjavanames = 
    ( "Calendar.JANUARY",
      "Calendar.FEBRUARY",
      "Calendar.MARCH",
      "Calendar.APRIL",
      "Calendar.MAY",
      "Calendar.JUNE",
      "Calendar.JULY",
      "Calendar.AUGUST",
      "Calendar.SEPTEMBER",
      "Calendar.OCTOBER",
      "Calendar.NOVEMBER",
      "Calendar.DECEMBER" );

sub parseRule($$$) {
    my ($rawoffset, $stdoffset, $rule) = @_;
    my $monthnr = parseMonth($rule->[0]);
    my $time  = parseTime($rawoffset, $stdoffset, $rule->[2]);
    my $dayoffset = 0;
    while ($time < 0) {
	$time += 24*3600*1000;
	$dayoffset--;
    }
    while ($time > 24*3600*1000) {
	$time -= 24*3600*1000;
	$dayoffset++;
    }
    $day = parseDay($dayoffset, $monthnr, $rule->[1]);
    return [ $monthjavanames[$monthnr-1], $day, $time ];
}


sub ruleEquals($$) {
    my ($rule1, $rule2) = @_;
    # check month names
    return (($rule1->[0] eq $rule2->[0])
	    && ($rule1->[1] eq $rule2->[1])
	    && ($rule1->[2] == $rule2->[2]));
}

sub findAlias($$) {
    my ($rawoffset, $rule) = @_;
    foreach $tz (@timezones) {
	my ($key, $tzoffset, $tzrule, $aliaslist) = @{$tz};
	next if ($tzoffset != $rawoffset);
	if ($rule eq "-") {
	    return $tz if ($tzrule eq "-");
	} elsif ($tzrule ne "-") {
	    next if $rule->[2] != $tzrule->[2];
	    if (ruleEquals($rule->[0], $tzrule->[0])
		&& ruleEquals($rule->[1], $tzrule->[1])) {
		return $tz;
	    }
	}
    }
    return "";
}

sub makePretty($) {
    my ($offset) = @_;
    if (($offset % 3600) == 0) {
	$offset /= 3600;
	return "$offset * 3600";
    } else {
	return "$offset";
    }
}

sub tzcompare($$) {
    my ($a, $b) = @_;
    if (($a =~ /\//) != ($b =~ /\//)) {
      return ($a =~ /\//) ? 1 : -1;
    } else {
      return $a cmp $b;
    }
}

foreach $file (@TIMEZONEFILES) {
#    print STDERR "$file\n";
    open INPUT, "$TIMEZONEDIR/$file" or die "Can't open $TIMEZONEDIR/$file";
    my $in_time_zone = 0;
    while (<INPUT>) {
	$_ = $1 if /^([^\#]*)\#/;
	next if /^\s*$/;
	my @entries = split;
#	$, = ","; print "'$_' -> [",@entries,"]\n";
	if (!$in_time_zone) {
	    if ($entries[0] eq "Rule") {
		# check if rule still applies
		# column 3 is TO entry.
		if ($entries[3] eq "max") {
		    my $rulename = $entries[1];
		    my $month = $entries[5];
		    my $day  = $entries[6];
		    my $time = $entries[7];
		    if ($entries[8] eq "0") {
			# This is the end time rule
			$rules{"$rulename"}[0] = [ $month, $day, $time ];
		    } else {
			# This is the start time rule
			$rules{"$rulename"}[1] = [ $month, $day, $time ];
			$rules{"$rulename"}[2] = parseOffset($entries[8]);
		    }
		}
	    } elsif ($entries[0] eq "Zone") {
		$in_time_zone = 1;
		shift @entries;
		$timezonename = shift @entries;
	    } elsif ($entries[0] eq "Remove") {
		my $found = 0;
		foreach $tz (@timezones) {
		    my @newaliases;
		    foreach $tzname (@{$tz->[3]}) {
			if ($tzname eq $entries[1]) {
			    $found = 1;
			} else {
			    push @newaliases, $tzname;
			}
		    }
		    if ($found) {
			if ($tz->[0] eq $entries[1]) {
			  $tz->[0] = $newaliases[0];
			}
			$tz->[3] = \@newaliases;
			last;
		    }
		}

		die "Unknown link $_" if ! $found;
	    } elsif ($entries[0] eq "Link") {
		my $alias = 0;
		foreach $tz (@timezones) {
		    foreach $tzname (@{$tz->[3]}) {
			if ($tzname eq $entries[1]) {
			    $alias = $tz;
			    last;
			}
		    }
		}

		die "Unknown link $_" if ! $alias;
	        die "@entries" if $entries[1] =~ /^\d+$/;
		push @{$alias->[3]}, $entries[2];
	    } else {
		die "Unknown command: $_";
	    }
	}
	if ($in_time_zone) {
	    die "early end of Zone: $_" if ($entries[0] =~ /^[A-Za-z]+/);
	    if (@entries <= 3) {
#		print "found ZONE $timezonename $entries[0] $entries[1] $entries[2]\n";
		# This is the last line and the only we look at.
		# other lines are for historic time zones.
		my $rawoffset = parseOffset($entries[0]);
		my $rule = $rules{"$entries[1]"} || "-";
		if ($rule ne "-") {
		    if (!defined($rule->[2])) {
			$rule = "-";
		    } else {
			# now we can parse the time since we know raw offset.
			my $savings = $rule->[2];
			my $endrule = parseRule($rawoffset, $savings, 
						$rule->[0]);
			my $startrule = parseRule($rawoffset, $savings, 
						  $rule->[1]);
			$rule = [ $endrule, $startrule, $savings ];
#			print "start",@{$rule->[1]}, "end", @{$rule->[0]}, 
#			"offset", $rule->[2],"\n";
		    }
		}
		my $alias = findAlias($rawoffset, $rule);
		if ($alias) {
		    if (($alias->[0] =~ /\//)
			&& ($timezonename !~ /\//)) {
			# alias is of Country/City form, timezonename not
			# make timezonename the real zone name
			$alias->[0] = $timezonename;
		    }
		    push @{$alias->[3]}, $timezonename;
		} else {
		    push @timezones, [ $timezonename, $rawoffset, $rule, 
				       [ $timezonename ] ];
		}
		$in_time_zone = 0;
	    }
	}
    }
    close INPUT;
}

@timezones = sort { if ($a->[1] != $b->[1]) { $a->[1] <=> $b->[1] } 
		    else { $a->[0] cmp $b->[0] } } @timezones;
for (@timezones) {
    my ($name, $rawoffset, $rule, $aliaslist) = @{$_};
    my @aliases = sort { tzcompare($a, $b); } @{$aliaslist};
    $name = $aliases[0];
    $rawoffset = makePretty($rawoffset);
    if ($rule eq "-") {
	print <<EOF
	tz = new SimpleTimeZone($rawoffset, \"$name\");
EOF
    } else {
	my ($endmonth, $endday, $endtime) = @{$rule->[0]};
	my ($startmonth, $startday, $starttime) = @{$rule->[1]};
	$endtime = makePretty($endtime);
	$starttime = makePretty($starttime);
	my $savings = $rule->[2];
	if ($savings == 3600 * 1000) {
	    print <<EOF
	tz = new SimpleTimeZone
	  ($rawoffset, \"$name\",
	   $startmonth, $startday, $starttime,
	   $endmonth, $endday, $endtime);
EOF
	} else {
	    $savings = makePretty($savings);
	    print <<EOF
	tz = new SimpleTimeZone
	  ($rawoffset, \"$name\",
	  $startmonth, $startday, $starttime,
	  $endmonth, $endday, $endtime, $savings);
EOF
        }
    }
    for (@aliases) {
    print <<EOF
	timezones0.put(\"$_\", tz);
EOF
    }
}


