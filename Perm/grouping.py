#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 11 20:55:19 2017

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')
    
from location import getLocation
from fileio import get, save
from fsio import setFile


####################################################################################
#
# Groupings
#
####################################################################################
def getGroupings():
    filename = setFile(getLocation(),"groupings.yaml")
    return get(filename)

def saveGroupings(yamlgroupings):
    filename = setFile(getLocation(),"groupings.yaml")
    save(filename, yamlgroupings)

    
 
####################################################################################
#
# Mapping Functions
#
####################################################################################
def getGroupMapping(debug = False):
    groupdata = getGroupings()
    if groupdata == None:
        raise ValueError("Groupdata is None. Need to Re-Source")
    categoryToGroup = {}
    categoryToMidGroup = {}
    categoryToSuperGroup = {}

    groupToMidGroup = {}
    midGroupToSuperGroup = {}
    
    categoryMap = {}
    
    if not isinstance(groupdata, list):
        raise ValueError("There is an error in the groupdata type",type(groupdata))
        
    for item in groupdata:

        if not isinstance(item, dict):
            raise ValueError("There is an error in the item type",type(item))
            
        for itemname,itemdata in item.iteritems():
            grouptype = "Group"
            if itemname == "MidGroupings":
                grouptype = "MidGroup"
            if itemname == "SuperGroupings":
                grouptype = "SuperGroup"
            
            if not isinstance(itemdata, list):
                raise ValueError("There is an error in the itemdata type",type(itemdata))
            
            #print itemname,'\t',type(itemdata),len(itemdata)
            for groupitem in itemdata:                
                #print type(groupitem),len(groupitem),grouptype
                          
                if not isinstance(groupitem, dict):
                    raise ValueError("There is an error in the itemdata type",type(groupitem))
                    
                for groupname,groupdata in groupitem.iteritems():
                    #print groupname,'\t',type(groupdata),'\t',len(groupdata)

                    if isinstance(groupdata, str) or isinstance(groupdata, unicode):
                        if grouptype != "Group":
                            raise ValueError("There is an error how the groups are ordered")
                        subgroup = itemname
                        category = groupname
                        categoryToGroup[category] = subgroup
                        if debug:
                            print "Category[",category,"] --->",subgroup
                    elif isinstance(groupdata, list):
                        for groupdataitem in groupdata:
                            
                            if not isinstance(groupdataitem, str):
                                raise ValueError("There is an error in the groupdataitem type",type(groupdataitem))
                            if grouptype == "MidGroup":
                                midgroup = groupname
                                subgroup = groupdataitem
                                groupToMidGroup[subgroup] = midgroup
                                if debug:
                                    print "Group[",subgroup,"] --->",midgroup
                            if grouptype == "SuperGroup":
                                supergroup = groupname
                                midgroup   = groupdataitem
                                midGroupToSuperGroup[midgroup] = supergroup
                                if debug:
                                    print "MidGroup[",midgroup,"] --->",supergroup
                    else:
                        raise ValueError("There is an error in the groupdata type",type(groupdata))


    ## Fill Category -> MidGroup and Category -> SuperGroup
    for category,group in categoryToGroup.iteritems():
        
        midgroup = groupToMidGroup.get(group)
        if midgroup == None:
            raise ValueError("There is no MidGroup for Group:",group,"and category",category)
            
        supergroup = midGroupToSuperGroup.get(midgroup)
        if supergroup == None:
            raise ValueError("There is no SuperGroup for MidGroup:",midgroup,",Group:",group,"and category",category)

        categoryMap[category] = {"Group": group, "MidGroup": midgroup, "SuperGroup": supergroup}
        categoryToMidGroup[category]   = midgroup
        categoryToSuperGroup[category] = supergroup
         
        if debug:
            print category,'-->',group,'-->',midgroup,'-->',supergroup


    ## Return Map        
    return categoryMap


 
####################################################################################
#
# Get List of Categories/Groups/MidGroups/SuperGroups
#
####################################################################################
def getUniqueGroupings(debug = False):
    categoryMap = getGroupMapping()
    categories  = categoryMap.keys()
    groups      = {}
    midgroups   = {}
    supergroups = {}
    for category,grouping in categoryMap.iteritems():
        groups[grouping["Group"]] = 1
        midgroups[grouping["MidGroup"]] = 1
        supergroups[grouping["SuperGroup"]] = 1
    groups = groups.keys()
    midgroups = midgroups.keys()
    supergroups = supergroups.keys()
 
    return categories, groups, midgroups, supergroups

    

####################################################################################
#
# Apply Groups
#
####################################################################################
def applyGrouping(records, basedir = "/Users/tgadfort/Documents/finance"):
    categoryMap = getGroupMapping()

    if isinstance(records, dict):
        for hashval,record in records.iteritems():
            try:
                category=record['category']
            except:
                raise ValueError("Could not get category",record.get('category'),"from record.")
            
            categoryData = categoryMap.get(category)
            if categoryData == None:
                print record
                raise ValueError("Could not find category:",category)

            group      = categoryData["Group"]
            midgroup   = categoryData["MidGroup"]
            supergroup = categoryData["SuperGroup"]
            
            record['group']      = group
            record['midgroup']   = midgroup
            record['supergroup'] = supergroup
                  
    if isinstance(records, list):
        for record in records:
            try:
                category=record['category']
            except:
                raise ValueError("Could not get category",record.get('category'),"from record.")
            
            categoryData = categoryMap.get(category)
            if categoryData == None:
                raise ValueError("Could not find category:",category)

            group      = categoryData["Group"]
            midgroup   = categoryData["MidGroup"]
            supergroup = categoryData["SuperGroup"]
            
            record['group']      = group
            record['midgroup']   = midgroup
            record['supergroup'] = supergroup

    return records