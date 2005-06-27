/** 
* @file sample_resize_trigger.hpp 
* Contains a sample resize trigger policy class.
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


#ifndef SAMPLE_RESIZE_TRIGGER_HPP
#define SAMPLE_RESIZE_TRIGGER_HPP


/**
* <class 
*   description = "A sample resize trigger policy."
*  comment = "This class serves to show the interface a trigger policy
*	needs to support.">
**/
class sample_resize_trigger
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
		sample_resize_trigger
		();
	/** </fn> **/

	/*
	* <fn description = "Copy constructor."
	*	comment = "Must be copy constructible.">
	**/
		sample_range_hashing
		(const sample_resize_trigger &r_other);
	/** </fn> **/

	/*
	* <fn description = "Swaps content."
	*	comment = "Must be swappable (if there is such a word).">
	**/
	inline void
		swap
		(sample_resize_trigger &r_other);
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
	* <fn description = "Notifies an element was inserted.
	*	the total number of entries in the table is num_entries.">
	**/
	inline void
		notify_inserted
		(size_type num_entries);
	/** </fn> **/

	/*
	* <fn description = "Notifies an element was erased."
	*	the total number of entries in the table is num_entries.">
	**/
	inline void
		notify_erased
		(size_type num_entries);
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
	* <fn description = "Notifies the table was resized as a result of
	*   this object's signifying that a resize is needed."
	*	The actual size of the table is new_size.">
	**/
	void
		notify_resized
		(size_type new_size);
	/** </fn> **/

	/*
	* <fn description = "Notifies the table was resized externally."
	*	The actual size of the table is new_size.">
	**/
	void
		notify_externally_resized
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
	* <fn description = "Queries whether a resize is needed.">
	**/
	inline bool 
		is_resize_needed
		() const;
	/** </fn> **/

	/*
	* <fn description = "Queries whether a grow is needed."
	*   comment = "This method is
	*   called only if this object indicated resize is needed.
	*	The actual size of the table is size, and the number of
	*	entries in it is num_entries.">
	**/
	inline bool
		is_grow_needed
		(size_type size, 
			size_type num_entries) const;
	/** </fn> **/

	/*
	* <fn description = "Queries whether a shrink is needed."
	*   comment = "This method is
	*   called only if this object indicated resize is needed.
	*	The actual size of the table is size, and the number of
	*	entries in it is num_entries.">
	**/
	inline bool
		is_shrink_needed
		(size_type size, 
			size_type num_entries) const;
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


/*
****************************************************************************
****************************************************************************
*/
} // namespace pb_assoc


#endif // #ifndef SAMPLE_RESIZE_TRIGGER_HPP
