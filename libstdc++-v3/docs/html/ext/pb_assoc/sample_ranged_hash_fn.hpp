/** 
* @file sample_ranged_hash_fn.hpp
* Contains a ranged hash policy.
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


#ifndef SAMPLE_RANGED_HASH_FN_HPP
#define SAMPLE_RANGED_HASH_FN_HPP


/**
* <class 
*   description = "A sample ranged-hash functor."
*  comment = "This class serves to show the interface a ranged-hash 
*	functor needs to support.">
**/
class sample_ranged_hash_fn
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
		sample_ranged_hash_fn
		();
	/** </fn> **/

	/*
	* <fn description = "Copy constructor."
	*	comment = "Must be copy constructible.">
	**/
		sample_ranged_hash_fn
		(const sample_ranged_hash_fn &r_other);
	/** </fn> **/

	/*
	* <fn description = "Swaps content."
	*	comment = "Must be swappable (if there is such a word).">
	**/
	inline void
		swap
		(sample_ranged_hash_fn &r_other);
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
	** <group 
	*	  description = "Notification methods.">
	**/
	
	/*
	* <fn 
	*   description = "Notifies the policy object that the container's
	*	  __size has changed to size."
	*	comment = "@@sample_ranged_hash_fn_size_type_comment">
	**/ 
	void 
		notify_resized
		(size_type size);
	/** </fn> **/

	/**
	***************************************************************************
	***************************************************************************
	** </group>
	**/


	/**
	***************************************************************************
	***************************************************************************
	** <group 
	*	  description = "Operators.">
	**/
	
	/*
	* <fn description = "Transforms the const key reference 
	*		r_key into a position
	*		within the table.">
	*	comment = "@@sample_ranged_hash_fn_size_type_comment">
	**/ 
	inline size_type 
		operator()
		(const_key_reference r_key) const;
	/** </fn> **/

	/**
	***************************************************************************
	***************************************************************************
	** </group>
	**/
};
/**
* </class>
**/


#endif // #ifndef SAMPLE_RANGED_HASH_FN_HPP
