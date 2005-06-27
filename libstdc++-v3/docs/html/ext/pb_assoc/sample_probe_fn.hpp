/** 
* @file sample_probe_fn.hpp
* Contains a sample probe policy.
* 
* Copyright Ami Tavory, IBM-HRL, 2004.
* 
* Permission to use, copy, modify, sell, and distribute this software
*   is hereby granted without fee, provided that the above copyright notice
*   appears in all copies, and that both that copyright notice and this
*   permission notice appear in supporting documentation.
*
* None of the above authors, nor IBM Haifa Research Laboratories, make any
*   representation about the suitability of this software for any
*   purpose. It is provided "as is" without express or implied warranty.
**/


#ifndef SAMPLE_PROBE_FN_HPP
#define SAMPLE_PROBE_FN_HPP


/**
* <class 
*   description = "A sample probe policy."
*  comment = "This class serves to show the interface a probe functor
*	needs to support.">
**/
class sample_probe_fn
{
/**
*******************************************************************************
*******************************************************************************
* <public_methods_group>
**/
public:
	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Constructors, destructor, and related.">
	**/

	/*
	* <fn description = "Default constructor."
	*	comment = "Must be default constructible.">
	**/
		sample_probe_fn
		();
	/** </fn> **/

	/*
	* <fn description = "Copy constructor."
	*	comment = "Must be copy constructible.">
	**/
		sample_probe_fn
		(const sample_probe_fn &r_other);
	/** </fn> **/

	/*
	* <fn description = "Swaps content."
	*	comment = "Must be swappable (if there is such a word).">
	**/
	inline void
		swap
		(sample_probe_fn &r_other);
	/** </fn> **/

	/*
	***************************************************************************
	***************************************************************************
	* </group>
	**/


/**
*******************************************************************************
*******************************************************************************
* </public_methods_group>
**/


/**
*******************************************************************************
*******************************************************************************
* <protected_methods_group>
**/
protected:
	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Offset methods.">
	**/

	/*
	* <fn description = "Returns the i-th offset from the hash value
	*		of some key r_key."
	*	comment = "@@sample_range_hashing_size_type_comment">
	**/
	inline size_type
		operator()
		(const_key_reference r_key,
			size_type i) const;
	/** </fn> **/

	/**
	***************************************************************************
	***************************************************************************
	** </group">
	**/

/**
*******************************************************************************
*******************************************************************************
* </protected_methods_group>
**/
};
/**
* </class>
**/


#endif // #ifndef SAMPLE_PROBE_FN_HPP
