/** 
* @file sample_update_policy.hpp
* Contains a sample policy for list update containers.
* 
* Copyright Ami Tavory, IBM-HRL, 2004.
* 
* Permission to use, copy, modify, sell, and distribute this software
*	is hereby granted without fee, provided that the above copyright notice
*	appears in all copies, and that both that copyright notice and this
*	permission notice appear in supporting documentation.
*
* None of the above authors, nor IBM Haifa Research Laboratories, make any
*	representation about the suitability of this software for any
*	purpose. It is provided "as is" without express or implied warranty.
**/


#ifndef SAMPLE_UPDATE_POLICY_HPP
#define SAMPLE_UPDATE_POLICY_HPP


/**
* <class
*	description = "A sample list-update policy."
*  comment = "This class serves to show the interface a list update functor
*	needs to support.">
**/
class sample_update_policy
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
		sample_update_policy
		();
	/** </fn> **/

	/*
	* <fn description = "Copy constructor."
	*	comment = "Must be copy constructible.">
	**/
		sample_update_policy
		(const sample_update_policy &r_other);
	/** </fn> **/

	/*
	* <fn description = "Swaps content."
	*	comment = "Must be swappable (if there is such a word).">
	**/
	inline void
		swap
		(sample_update_policy &r_other);
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
* <protected_types_group>
**/
protected:

	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Metadata definitions.">
	**/

	/**
	* <tdef description = "Metadata on which this functor operates."
	*	comment = "The class must declare the metadata type on which it
	*		operates; the list-update based containers will append to 
	*		each node an object of this type.">
	**/
	typedef
		some_metadata_type
		metadata_type;
	/** </tdef> **/


	/*
	***************************************************************************
	***************************************************************************
	* </group>
	**/

/**
*******************************************************************************
*******************************************************************************
* </protected_types_group>
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
	** <group description = "Metadata operations.">
	**/

	/*
	* <fn description = "Creates a metadata object."
	*	comment = "A list-update based container object  will call this
	*		method to create a metadata type when a node is created.">
	**/
	metadata_type
		operator()
		() const;
	/** </fn> **/

	/*
	* <fn description = "Decides whether a metadata object should be 
	*	moved to the front of the list. A list-update based containers object 
	*	will call this
	*	method to decide whether to move a node to the front of 
	*	the list. The method shoule return true if the node should
	*	be moved to the front of the list."
	*	comment = "@@sample_update_policy_metadata_reference">
	**/
	bool 
		operator()
		(metadata_reference r_data) const;
	/** </fn> **/

	/*
	***************************************************************************
	***************************************************************************
	* </group>
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


#endif // #ifndef SAMPLE_UPDATE_POLICY_HPP
