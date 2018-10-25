#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon May  1 17:20:15 2017

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

from fileio import get, save
from fsio import setSubDirFile, moveFile
from search import removeSubPattern, findSubExt
from uid import getHash
from strdate import getTimeline, getDate, getDateText, getMDY
from show import getSummary
from grouping import applyGrouping
from getter import formatRecords


###############################################################################
#
# Parse Original CSV File
#
###############################################################################
def parseCSVdata(row, account):    
    output={}
    keys   = ['date', 'payee', 'category', 'statement', 'original', 'amount', 'type', 'transfer', 'entry']
    for k in keys:
        output[k] = row[k.title()]
    output['account'] = account
    output['date']    = getDate(output['date'])

    mon,day,year     = getMDY(output['date'])
    output['month']  = int(mon)
    output['day']    = int(day)
    output['year']   = int(year)
    output['amount'] = float(output['amount'])    

    keys=getHash(output)

    return keys,output



###############################################################################
#
# Write CSV File
#
###############################################################################
def writeCSVFile(data, account,
                 basedir = '/Users/tgadfort/Documents/finance', debug = False):
    startdate,enddate=getTimeline(data)
    basename = getDateText(startdate, enddate)+".csv"
    filename = setSubDirFile(basedir, [account, "csv"], basename)
    f=open(filename, "w")
    writeLine(f, "Entry", "Statement", "Date", "Amount", "Category", "Payee", "Type", "Transfer", "Original")
    nline=0
    for i in range(len(data)):
        writeLine(f, i, data[i]['statement'], data[i]['date'], data[i]['amount'], data[i]['category'], data[i]['payee'], data[i]['type'], data[i]['transfer'], data[i]['original'])
        nline += 1
    print "\tWrote",nline,"lines to",filename
    f.close()



###############################################################################
#
# Write CSV File from Dict
#
###############################################################################
def writeCSVFileFromDict(data, account, subdir = None, basename = None,
                         basedir = '/Users/tgadfort/Documents/finance', debug = False):
    startdate,enddate=getTimeline(data)
    if basename == None:
        basename = getDateText(startdate, enddate)+".csv"
    else:
        if basename.find(".csv") == -1:
            basename = basename + ".csv"

        
    if subdir:
        filename = setSubDirFile(basedir, [account, subdir], basename)
    else:
        filename = setSubDirFile(basedir, [account], basename)
        
    records = formatRecords(data, sort = True, returnList = True)
        
    f=open(filename, "w")    
    nline=0
    for i,record in enumerate(records):
        vals = [str(x) for x in record.values()]
        if i == 0:
            keys = record.keys()
            f.write(",".join(keys)+"\n")
        f.write(",".join(vals)+"\n")
        nline += 1
    print "\tWrote",nline,"lines to",filename
    f.close()
    



###############################################################################
#
# Write Line in CSV File
#
###############################################################################
def writeLine(outfile, nentries, statement, date, amount, category, payee, ptype, transfer, original):
    lout=[]
    lout.append(nentries)
    lout.append(statement)
    lout.append(date)
    lout.append(amount)
    lout.append(category)
    lout.append(payee)
    lout.append(ptype)
    lout.append(transfer)
    lout.append(original)
    lout=[str(x) for x in lout]
    outfile.write(",".join(lout))
    outfile.write("\n")
    


###############################################################################
#
# This is the output line to the json file
#
###############################################################################
def createJSON(account, basedir = '/Users/tgadfort/Documents/finance',
               debug = False):
    removeSubPattern(basedir, [account, "json"], ".json", debug)
    data={}
    files = findSubExt(basedir, [account, "csv"], ".csv")
    for ifile in sorted(files):
        csvdata = get(ifile)
        for row in csvdata:
            keys,vals=parseCSVdata(row, account)
            for j in range(len(keys)):
                if data.get(keys[j]) == None:
                    vals['key'] = j
                    data[keys[j]] = vals
                    break
            if len(data) % 1000 == 0:
                print "\tProcessed",len(data),"entries."

    data = applyGrouping(records = data)
    
    print "Found",len(data),"entries."
    filename    = setSubDirFile(basedir, [account, "json"], "records.json")
    tmpfilename = setSubDirFile(basedir, [account, "json"], "records-tmp.json")
    try:
        print "Moving original",filename,"to",tmpfilename
        moveFile(filename, tmpfilename)
    except:
        print "No original",filename
    print "Writing",len(data),"entries to",filename
    save(filename, data)


###############################################################################
#
# Show JSON Records
#
###############################################################################
def showSummary(account, source = None, basedir = '/Users/tgadfort/Documents/finance',
                showYear = False, debug = False):
    if source == None:
        filename = setSubDirFile(basedir, [account, "json"], "records.json")
        data     = get(filename)
    else:
        data = source
        
    startdate,enddate=getTimeline(data)
    getSummary(startdate, enddate, account = account, data = data, 
               show = True, showYear = showYear)
    
    
###############################################################################
#
# Parse Row Info
#
###############################################################################
def initCSVoutput():
    output={}    
    output['date']      = None
    output['statement'] = None
    output['payee']     = None
    output['category']  = None
    output['amount']    = None
    output['type']      = None
    output['transfer']  = None
    output['original']  = None
    return output