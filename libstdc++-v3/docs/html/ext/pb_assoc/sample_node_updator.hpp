/* 
* @file sample_node_updator.hpp
* Contains a samle node update functor.
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


#ifndef SAMPLE_NODE_UPDATOR_HPP
#define SAMPLE_NODE_UPDATOR_HPP


/**
* <class 
*	description = "A sample node updator."
*  comment = "This class serves to show the interface a node update functor
*	needs to support.">
**/
class sample_node_updator
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
	** <group description = "Conclassors, declassor, and related.">
	**/

	/*
	* <fn description = "Default constructor."
	*	comment = "Must be default constructible.">
	**/
		sample_node_updator
		();
	/** </fn> **/

	/*
	* <fn description = "Copy constructor."
	*	comment = "Must be copy constructible.">
	**/
		sample_node_updator
		(const sample_node_updator &r_other);
	/** </fn> **/

	/*
	* <fn description = "Swaps content."
	*	comment = "Must be swappable (if there is such a word).">
	**/
	inline void
		swap
		(sample_node_updator &r_other);
	/** </fn> **/

	/*
	***************************************************************************
	***************************************************************************
	* </group>
	**/


	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Operators.">
	**/

	/**
	* <fn description = "This method is called by the superclass container 
	*	object to update the key of a node whose invariants have been
	*	violated. p_key is a pointer to the key being updated;
	*	p_l_child_key is a pointer to the key of the left-child node of
	*	the node being updated (and is NULL if there is no such child node);
	*	p_r_child_key is a pointer to the key of the right-child node of
	*	the node being updated (and is NULL if there is no such child node)."
	*	comment = "@@sample_node_updator_const_key_pointer_comment">
	**/
	void
		operator()
		(const_key_pointer p_key, 
			const_key_pointer p_l_child_key, 
			const_key_pointer p_r_child_key);
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
};
/**
* </class>
**/


#endif // #ifndef SAMPLE_NODE_UPDATOR_HPP
