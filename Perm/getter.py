#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 12 17:16:06 2017

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Python')
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')

from location import getLocation, getBaseLocation
from fileio import get, save
from fsio import setFile, setSubFile
from uid import getHash
from strdate import writeDate



###############################################################################
#
# Edit Functions
#
###############################################################################
def getEdits(sort = False):
    savedfilename = setFile(getLocation(), "saved.json")
    fixed         = get(savedfilename)
    if sort:
        fixed = formatRecords(fixed, sort = True)
    return fixed

def getEdit(record):
    fixed    = getEdits()
    hashval  = getHash(record)
    value    = fixed.get(hashval)
    return value

def saveEdits(fixed, basedir = "/Users/tgadfort/Documents/finance"):
    savedfilename = setFile(getLocation(), "saved.json")
    save(savedfilename, fixed, debug=True)
    
def getData(sort = False):
    savedfilename = setSubFile(getBaseLocation(), "Portfolio", "records.json")
    records       = get(savedfilename)
    if sort:
        records = formatRecords(records, sort = True)
    return records

def saveData(data):
    savedfilename = setSubFile(getBaseLocation(), "Portfolio", "records.json")
    save(savedfilename, data, debug=True)



###############################################################################
#
# Formatting/Sorting Functons
#
###############################################################################
def formatRecords(records, sort = True, returnList = False):
    newRecords = {}

    if isinstance(records, dict):
        listRecords=[]
        for k,v in records.iteritems(): listRecords.append(v)
    elif isinstance(records, list):
        listRecords = records
    else:
        raise ValueError("Did not understand type",type(records),"for sorting")

    if sort:
        sortedRecords = sortRecords(listRecords)
    else:
        sortedRecords = listRecords

    if returnList:
        return sortedRecords
        
    for record in sortedRecords:
        hashval = getHash(record)
        newRecords[hashval] = record

    return newRecords

    
def sortRecords(records):
    recs={}
    if isinstance(records, dict):
        listRecords=[]
        for k,v in records.iteritems(): listRecords.append(v)
    else:
        listRecords = records
    
    for rec in listRecords:
        if rec.get('date'):
            sdate = writeDate(rec['date'], dformat="%Y/%m/%d")
        elif rec.get('Date'):
            sdate = writeDate(rec['Date'], dformat="%Y/%m/%d")
        else:
            raise ValueError("No date or Date in record!")
            
        if recs.get(sdate) == None:
            recs[sdate] = []
        recs[sdate].append(rec)
    vals=sorted(recs.keys())
    srecs=[]
    for rec in vals:
        dvals=recs[rec]
        for dval in dvals:
            srecs.append(dval)
    return srecs

