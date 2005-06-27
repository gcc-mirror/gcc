/** 
* @file sample_resize_policy.hpp
* Contains a sample resize policy for hash tables.
* 
* Copyright Ami Tavory, IBM-HRL, 2004.
* 
* Permission to use, copy, modify, sell, and distribute this software
*   is hereby granted without fee, provided that the above copyright notice
*   appears in all copies, and that both that copyright notice and this
*   permission notice appear in supporting documentation,
*
* None of the above authors, nor IBM Haifa Research Laboratories, make any
*   representation about the suitability of this software for any
*   purpose. It is provided "as is" without express or implied warranty.
**/


#ifndef SAMPLE_RESIZE_POLICY_HPP
#define SAMPLE_RESIZE_POLICY_HPP


/**
* <class 
*   description = "A sample resize policy."
*  comment = "This class serves to show the interface a resize policy
*	needs to support.">
**/
class sample_resize_policy
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
		sample_resize_policy
		();
	/** </fn> **/

	/*
	* <fn description = "Copy constructor."
	*	comment = "Must be copy constructible.">
	**/
		sample_range_hashing
		(const sample_resize_policy &r_other);
	/** </fn> **/

	/*
	* <fn description = "Swaps content."
	*	comment = "Must be swappable (if there is such a word).">
	**/
	inline void
		swap
		(sample_resize_policy &r_other);
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
	** <group description = "Insert search notifications."
	**  comment = "Notifications called during an insert operation.">
	**/

	/*
	* <fn description = "Notifies a search started.">
	**/
	inline void
		notify_insert_search_start
		();
	/** </fn> **/

	/*
	* <fn description = "Notifies a search encountered a collision.">
	**/
	inline void
		notify_insert_search_collision
		();
	/** </fn> **/

	/*
	* <fn description = "Notifies a search ended.">
	**/
	inline void
		notify_insert_search_end
		();
	/** </fn> **/

	/*
	***************************************************************************
	***************************************************************************
	* </group>
	**/


	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Find search notifications."
	**  comment = "Notifications called during a find operation.">
	**/

	/*
	* <fn description = "Notifies a search started.">
	**/
	inline void
		notify_find_search_start
		();
	/** </fn> **/

	/*
	* <fn description = "Notifies a search encountered a collision.">
	**/
	inline void
		notify_find_search_collision
		();
	/** </fn> **/

	/*
	* <fn description = "Notifies a search ended.">
	**/
	inline void
		notify_find_search_end
		();
	/** </fn> **/

	/*
	***************************************************************************
	***************************************************************************
	* </group>
	**/


	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Erase search notifications."
	**  comment = "Notifications called during an insert operation.">
	**/

	/*
	* <fn description = "Notifies a search started.">
	**/
	inline void
		notify_erase_search_start
		();
	/** </fn> **/

	/*
	* <fn description = "Notifies a search encountered a collision.">
	**/
	inline void
		notify_erase_search_collision
		();
	/** </fn> **/

	/*
	* <fn description = "Notifies a search ended.">
	**/
	inline void
		notify_erase_search_end
		();
	/** </fn> **/

	/*
	***************************************************************************
	***************************************************************************
	* </group>
	**/


	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Content change notifications."
	**  comment = "Notifications called when the content of the table
	**	  changes in a way that can affect the resize policy.">
	**/

	/*
	* <fn description = "Notifies an element was inserted.">
	**/
	inline void
		notify_inserted
		(size_type num_e);
	/** </fn> **/

	/*
	* <fn description = "Notifies an element was erased.">
	**/
	inline void
		notify_erased
		(size_type num_e);
	/** </fn> **/

	/*
	* <fn description = "Notifies the table was cleared.">
	**/
	void 
		notify_cleared
		();
	/** </fn> **/

	/*
	***************************************************************************
	***************************************************************************
	* </group>
	**/


	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Size change notifications."
	**  comment = "Notifications called when the table changes size.">
	**/

	/*
	* <fn description = "Notifies the table was resized to new_size.">
	**/
	void
		notify_resized
		(size_type new_size);
	/** </fn> **/

	/*
	***************************************************************************
	***************************************************************************
	* </group>
	**/


	/**
	***************************************************************************
	***************************************************************************
	** <group description = "Queries."
	**  comment = "Called to query whether/how to resize.">
	**/

	/*
	* <fn description = "Queries initial size.">
	**/
	size_type
		get_init_size
		() const;
	/** </fn> **/

	/*
	* <fn description = "Queries whether a resize is needed.">
	**/
	inline bool
		is_resize_needed
		() const;
	/** </fn> **/

	/*
	* <fn description = "Queries what the new size should be.">
	**/
	size_type
		get_new_size
		(size_type size, size_type num_used_e) const;
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


#endif // #ifndef SAMPLE_RESIZE_POLICY_HPP
