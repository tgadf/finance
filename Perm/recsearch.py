#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 12 17:04:34 2017

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

from strdate import matchDate, matchDateRange
from getter import getEdits, getData, formatRecords


###############################################################################
#
# Search Functions
#
###############################################################################
def passCut(record, cutName, cutValue):
    if cutValue == None:
        return True
    
    if cutName:
        value = record.get(cutName)
        if value == None:
            if "None" in cutValue:
                return True
            else:
                return False

        if isinstance(cutValue, list):
            if cutName.find("Date") != -1 or cutName.find("date") != -1:
                if not matchDateRange(value, cutValue):
                    return False
            elif isinstance(cutValue[0],int) or isinstance(cutValue[0],float):
                if value < cutValue[0] or value > cutValue[1]:
                    return False
            else:
                if value not in cutValue:
                    return False
        else:
            if cutName.find("Date") != -1 or cutName.find("date") != -1:
                if not matchDate(value, cutValue):
                    return False
            elif cutName.find("Amount") != -1 or cutName.find("amount") != -1:
                retval = value - cutValue
                if retval > 1 or retval < -1:
                    return False
            elif cutName.find("Original") != -1 or cutName.find("original") != -1:
                if value.find(cutValue) == -1:
                    return False
            elif cutName.find("Comment") != -1 or cutName.find("comment") != -1:
                if value.find(cutValue) == -1:
                    return False
            else:
                if value != cutValue:
                    return False
    else:
        print cutName,cutValue
        return False
        
                
    return True

        
def findRecords(basedir = "/Users/tgadfort/Documents/finance", source = None,
                cutPayee = None, cutCategory = None, cutDate = None,
                cutStatement = None, cutMonth = None, cutDay = None,
                cutAmount = None, cutOriginal = None, cutTransfer = None,
                cutAccount = None, cutYear = None, cutType = None,
                cutGroup = None, cutMidGroup = None, cutSuperGroup = None,
                debug = False, sort = True, returnKeys = False):
    if source == None:
        records = getData(basedir)
    else:
        if isinstance(source, dict):
            records = source
        elif isinstance(source, list):
            records = source
        elif source == "fixed":
            records = getEdits(basedir)
        else:
            records = getData(basedir)

    foundRecords = []

    for hashval,record in records.iteritems():
        result = []
        result.append(passCut(record, "payee", cutPayee))
        result.append(passCut(record, "category", cutCategory))
        result.append(passCut(record, "date", cutDate))
        result.append(passCut(record, "year", cutYear))
        result.append(passCut(record, "month", cutMonth))
        result.append(passCut(record, "day", cutDay))
        result.append(passCut(record, "statement", cutStatement))
        result.append(passCut(record, "amount", cutAmount))
        result.append(passCut(record, "original", cutOriginal))
        result.append(passCut(record, "transfer", cutTransfer))
        result.append(passCut(record, "account", cutAccount))
        result.append(passCut(record, "type", cutType))
        result.append(passCut(record, "group", cutGroup))
        result.append(passCut(record, "midgroup", cutMidGroup))
        result.append(passCut(record, "supergroup", cutSuperGroup))
        if all(result):
            if returnKeys:
                foundRecords.append(hashval)
            else:
                foundRecords.append(record)
 
    if not returnKeys:
        foundRecords = formatRecords(foundRecords, sort = sort)
    
    if debug:
        print "Found",len(foundRecords),"from",len(records)
    
        
    return foundRecords