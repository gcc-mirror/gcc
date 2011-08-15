#!/usr/bin/python

import string
import sys
import re
from Numeric import *
from pychart import *
from xml.dom import minidom

class exception:
	pass


class res:
	"""
	A 'structure' representing the results of a test.
	"""
	def __init__(self, x_label, y_label, cntnr_list, cntnr_descs, res_sets):
		self.x_label = x_label
		self.y_label = y_label
		self.cntnr_list = cntnr_list
		self.cntnr_descs = cntnr_descs
		self.res_sets = res_sets


class res_getter:
	"""
	This class returns a res object for some test.
	"""
	class __sorter:
		def __accum(self, results):
			total = 0
			for result in results:
				total = total + result[1]
			return total

		def sort(self, cntnr_list, res_sets):
			cntnrs_and_totals = []
			for cntnr in cntnr_list:
				results = res_sets[cntnr]
				total = self.__accum(results)
				cntnrs_and_totals.append((cntnr, total))
			by_total = lambda x,y: x[1] > y[1] and -1 or 1
			cntnrs_and_totals.sort(by_total)
			ret = []
			for cntnr_and_total in cntnrs_and_totals:
				cntnr = cntnr_and_total[0]
				ret.append(cntnr)
			return ret

	def __init__(self, test_infos_f_name):
		self.__test_to_container_res_sets = {}
		self.__test_to_f_names = {}
		tests_dat = minidom.parse(test_infos_f_name)
		for test in tests_dat.getElementsByTagName('test'):
			test_name = test.attributes['name'].value
			self.__test_to_f_names[test_name] = test.getElementsByTagName('file')[0].attributes['name'].value
			cntnr_list = []
			for cntnr in test.getElementsByTagName('cntnr'):
				cntnr_list.append(cntnr.attributes['name'].value)
			self.__test_to_container_res_sets[test_name] = cntnr_list

	def __get_label(self, tst_dat, label_name):
		label = tst_dat.getElementsByTagName(label_name)[0].firstChild.data
		label = string.strip(label, '\n')
		label = string.strip(label)
		return label

	def __parse_result_sets(self, f_name, cntnr_list):
		tst_dat = minidom.parse(f_name)
		x_label = self.__get_label(tst_dat, 'x_name')
		y_label = self.__get_label(tst_dat, 'y_name')
		parsed_container_list = tst_dat.getElementsByTagName('cntnr')
		res_sets = {}
		cntnr_descs = {}
		for cntnr in parsed_container_list:
			cntnr_name = cntnr.attributes["name"].value
			res_sets[cntnr_name] = []
		for cntnr in parsed_container_list:
			cntnr_name = cntnr.attributes["name"].value
			cntnr_desc = cntnr.getElementsByTagName('desc')
			if res_sets.has_key(cntnr_name):
				res_set = []
				result_list = cntnr.getElementsByTagName('result')
				for result in result_list:
					x = string.atol(result.attributes["x"].value)
					y = string.atof(result.attributes["y"].value)
					res_set.append((x, y))
				res_sets[cntnr_name] = res_set
				cntnr_descs[cntnr_name] = cntnr_desc[0]
		return (x_label, y_label, cntnr_descs, res_sets)

	def get(self, res_dir, test_name):
		cntnr_list = self.__test_to_container_res_sets[test_name]
		f_name = res_dir + '/' + self.__test_to_f_names[test_name]
		parsed = self.__parse_result_sets(f_name, cntnr_list)
		x_label = parsed[0]
		y_label = parsed[1]
		cntnr_descs = parsed[2]
		res_sets = parsed[3]
		cntnr_list = self.__sorter().sort(cntnr_list, res_sets)
		return res(x_label, y_label, cntnr_list, cntnr_descs, res_sets)


class image_maker:
	"""
	This class creates a svg file from a result set.
	"""
	class __style_chooser:
		def __init__(self):
			self.native_re = re.compile(r'n_(?:.*?)')

			self.native_tick_mark_0 = tick_mark.blackdtri
			self.native_tick_mark_1 = tick_mark.blackdia
			self.native_line_style_0 = line_style.gray50_dash1
			self.native_line_style_1 = line_style.gray50_dash2

			self.mask_re = re.compile(r'mask(?:.*?)')
			self.mod_re = re.compile(r'mod(?:.*?)')

			self.rb_tree_mmap_rb_tree_set_re = re.compile(r'rb_tree_mmap_rb_tree_set(?:.*?)')
			self.rb_tree_mmap_lu_mtf_set_re = re.compile(r'rb_tree_mmap_lu_mtf_set(?:.*?)')

			self.splay_re = re.compile(r'splay(?:.*?)')
			self.rb_tree_re = re.compile(r'rb_tree(?:.*?)')
			self.ov_tree_re = re.compile(r'ov_tree(?:.*?)')
			self.splay_tree_re = re.compile(r'splay_tree(?:.*?)')

			self.pat_trie_re = re.compile(r'pat_trie(?:.*?)')

			self.lc_1div8_1div2_re = re.compile(r'lc_1div8_1div2(?:.*?)')
			self.lc_1div8_1div1_re = re.compile(r'lc_1div8_1div1(?:.*?)')
			self.mcolc_1div2_re = re.compile(r'mcolc_1div2(?:.*?)')

		def choose(self, cntnr):
			if self.native_re.search(cntnr):
				if cntnr == 'n_pq_vector':
					return (self.native_tick_mark_1, self.native_line_style_1)

				return (self.native_tick_mark_0, self.native_line_style_0)

			# tick_mark predefined
			# square, circle3, dia, tri, dtri, star, plus5, x5, gray70dia, blackdtri, blackdia
			if self.mask_re.search(cntnr):
				clr = color.navy
			elif self.mod_re.search(cntnr):
				clr = color.green4
			elif self.rb_tree_mmap_rb_tree_set_re.search(cntnr):
				clr = color.mediumblue
				tm = tick_mark.square
			elif self.rb_tree_mmap_lu_mtf_set_re.search(cntnr) or cntnr == 'rc_binomial_heap':
				clr = color.gray50
				tm = tick_mark.dia
			elif self.splay_tree_re.search(cntnr) or cntnr == 'binomial_heap':
				clr = color.gray58
				tm = tick_mark.tri
			elif self.rb_tree_re.search(cntnr) or cntnr == 'binary_heap':
				clr = color.red3
				tm = tick_mark.dtri
			elif self.ov_tree_re.search(cntnr) or cntnr == 'thin_heap':
				clr = color.orangered1
				tm = tick_mark.star
			elif self.pat_trie_re.search(cntnr) or cntnr == 'pairing_heap':
				clr = color.blueviolet
				tm = tick_mark.plus5
			else:
				sys.stderr.write(cntnr + '\n')
				raise exception

			# mask / mod
			if cntnr.find('lc_1div8_1div') <> -1:
				if cntnr.find('mask') <> -1:
					# mask
					if self.lc_1div8_1div2_re.search(cntnr):
						if cntnr.find('nsth') <> -1:
							tm = tick_mark.x5
						else:
							tm = tick_mark.gray70dia
					if self.lc_1div8_1div1_re.search(cntnr):
						if cntnr.find('nsth') <> -1:
							tm = tick_mark.dia
						else:
							tm = tick_mark.circle3
				else:
					# mod
					if self.lc_1div8_1div2_re.search(cntnr):
						if cntnr.find('nsth') <> -1:
							tm = tick_mark.tri
						else:
							tm = tick_mark.square
					if self.lc_1div8_1div1_re.search(cntnr):
						if cntnr.find('nsth') <> -1:
							tm = tick_mark.dtri
						else:
							tm = tick_mark.star

			if self.mcolc_1div2_re.search(cntnr):
				tm = tick_mark.circle3

			return (tm, line_style.T(color = clr, width = 2))


	def __init__(self):
		self.__sc = self.__style_chooser()
		self.__mmap_re = re.compile('mmap_')

	def __container_label_name(self, cntnr):
		return self.__mmap_re.sub('\nmmap_\n', cntnr)

	def make(self, res, of_name):
		print of_name

		# theme settings
		theme.debug_level = 3
		theme.output_format = 'svg'
		theme.scale_factor = 2
		theme.default_line_width = 0.5
		theme.default_font_size = 8
		theme.use_color = 1
		theme.reinitialize()

		# canvas settings
		f = file(of_name, "w")
		can = canvas.init(f, "svg")

		# axes
		y_tick_interval = self.__get_y_tics(res)
		xaxis = axis.X(format = "/6/i/a-90{}%d",
			       tic_interval = 200,
			       label = res.x_label, label_offset = (0, -20))
		yaxis = axis.Y(format = "/6/i/a0{}%.2e",
			       tic_interval = y_tick_interval, tic_label_offset = (-25, 0),
			       label = res.y_label, label_offset = (-15, 0))

		# legend
		legend_lines = len(res.cntnr_list)
		legend_vloc = 80 + (legend_lines * 10)
		legend_hloc = -0
		lg = legend.T(loc=(legend_hloc,-legend_vloc),
			      frame_line_style = None, inter_row_sep = 2)

		# plot datasets
		ar = area.T(x_axis = xaxis, y_axis = yaxis, legend = lg, size = (240,110), x_range = (0, 2200))
		plot_list = []
		for cntnr in res.cntnr_list:
			style = self.__sc.choose(cntnr)
			pl = line_plot.T(label = self.__container_label_name(cntnr),
					 data = res.res_sets[cntnr],
					 tick_mark = style[0],
					 line_style = style[1])
			plot_list.append(pl)

		for plot in plot_list:
			ar.add_plot(plot)

		# render image
		ar.draw(can)
		can.close()


	def __get_y_max_min(self, res):
		mx = 0
		nx = 0
		for cntnr in res.cntnr_list:
			m = max(d[1] for d in res.res_sets[cntnr])
			mx = max(m, mx)
			n = min(d[1] for d in res.res_sets[cntnr])
			nx = min(n, nx)
		return (mx, nx)

	def __get_x_max_min(self, res):
		mx = 0
		nx = 0
		for cntnr in res.cntnr_list:
			m = max(d[0] for d in res.res_sets[cntnr])
			mx = max(m, mx)
			n = min(d[0] for d in res.res_sets[cntnr])
			nx = min(n, nx)
		return (mx, nx)

	def __get_y_tics(self, res):
		mx = 0
		for cntnr in res.cntnr_list:
			m = max(d[1] for d in res.res_sets[cntnr])
			mx = max(m, mx)
		return mx / 5


def main(test_infos_f_name, res_dir, doc_dir):
	xmls_dat = minidom.parse(test_infos_f_name)
	for test in xmls_dat.getElementsByTagName('test'):

		# parse results
		test_name = test.attributes['name'].value
		res_gtr = res_getter(test_infos_f_name)
		res = res_gtr.get(res_dir, test_name)

		# generate image
		image_mkr = image_maker()
		svg_of_name = doc_dir + '/pbds_' + test_name + '.svg'
		image_mkr.make(res, svg_of_name)

if __name__ == "__main__":
	"""
	This module takes 3 parameters from the command line:
	Tests info XML file name
	Test results directory
	Image output directory
	"""
	usg = "make_graph.py <test_info_file> <res_dir> <image_dir>\n"
	if len(sys.argv) != 4:
		sys.stderr.write(usg)
		raise exception
	main(sys.argv[1], sys.argv[2], sys.argv[3])
