/** 
* @file sample_size_policy.hpp 
* Contains a sample size resize-policy.
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


#ifndef SAMPLE_SIZE_POLICY_HPP
#define SAMPLE_SIZE_POLICY_HPP


/**
* <class 
*   description = "A sample size policy."
*  comment = "This class serves to show the interface a size policy
*	needs to support.">
**/
class sample_size_policy
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
		sample_size_policy
		();
	/** </fn> **/

	/*
	* <fn description = "Copy constructor."
	*	comment = "Must be copy constructible.">
	**/
		sample_range_hashing
		(const sample_size_policy &r_other);
	/** </fn> **/

	/*
	* <fn description = "Swaps content."
	*	comment = "Must be swappable (if there is such a word).">
	**/
	inline void
		swap
		(sample_size_policy &r_other);
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
	**************************************************************************	
	***************************************************************************
	** <group description = "Size methods">
	**/
	
	/*
	* <fn description = "Given suggested_size, a suggested size, returns
	*   an initial size of the container related to the initial size.">
	**/ 
	inline size_type
		get_init_size
		(size_type suggested_size) const;
	/** </fn> **/
	
	/*
	* <fn description = "Given cur_size, the
	*	current size, returns a size that is 
	*   larger.">
	**/ 
	inline size_type
		get_nearest_larger_size
		(size_type cur_size) const;
	/** </fn> **/
	
	/*
	* <fn description = "Given cur_size, the current size, returns a size that is 
	*   smaller.">
	**/		 
	inline size_type
		get_nearest_smaller_size
		(size_type cur_size) const;		
	/** </fn> **/

	/**
	**************************************************************************	
	***************************************************************************
	** </group>
	**/


/**
*******************************************************************************
*******************************************************************************
* </protected_methods_group>
**/
};
/*
** </class>
**/


#endif // #ifndef SAMPLE_SIZE_POLICY_HPP
