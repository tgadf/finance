#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 11 16:46:59 2017

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Python')
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')

from copy import deepcopy
from fileio import get, save
from fsio import setFile, setSubFile
from location import getLocation
from uid import getHash, checkHash
from strdate import getDate, countDays, matchDateRange
from grouping import applyGrouping, getGroupMapping
from recsearch import findRecords
from getter import getEdits, getData, saveEdits



###############################################################################
#
# Apply Edits
#
###############################################################################
def applyEdit(record, fix, debug = False):
    fixes = 0
    
    
    ## check hash before altering
    hashval = getHash(record)
    if not isinstance(hashval, list):
        if not checkHash(rec=fix, hashval=hashval, quitOnError=False):
            print "Not applying edit because there is a problem with the hashes"
            print ""
            print record
            print "Record Hash ----->",getHash(record)
            print ""
            print fix
            print "Fix Hash    ----->",getHash(fix)
            print ""
            raise()
            return record
    
    r1 = deepcopy(record)
    h1 = getHash(r1)
    d1 = r1['date']
    for key in record.keys():
        #if key == "type": continue
        value = fix.get(key)
        if value:
            if record[key] != value:
                record[key] = value
                fixes += 1

    if debug and False:
        print "Applied edits to",fixes,"out of",len(record),"keys."                

    d2 = record['date']
    h2 = getHash(record)
    if d1 != d2:
        print "Error with dates!!!"
        print h1
        print d1
        print r1
        print h2
        print d2
        print record
        raise()
        
    return record, fixes

    
def applyEdits(records, basedir = "/Users/tgadfort/Documents/finance", debug = False):
    fixed = getEdits()
    known = {}
    fixes = 0
    if isinstance(records, list):
        fixedRecords = records
        for i,record in enumerate(records):
            key = record.get('key')
            hashval = getHash(record, key=key)
            if not isinstance(hashval,list):
                if fixed.get(hashval):                    
                    fixedRecords[i],nfixes = applyEdit(record, fixed[hashval], debug)
                    if nfixes: fixes += 1
            else:
                hashvals = hashval
                for j,hashval in enumerate(hashvals):
                    if known.get(hashval) == None:
                        known[hashval] = j
                        if fixed.get(hashval):
                            fixedRecords[i],nfixes = applyEdit(record, fixed[hashval], debug)
                            if nfixes: fixes += 1
                            break
                        
    if isinstance(records, dict):
        fixedRecords = records        
        for hashval,record in records.iteritems():
            if fixed.get(hashval):
                fixedRecords[hashval] = applyEdit(record, fixed[hashval], debug)
                fixes += 1
            
    if debug:
        print "\t  Applied edits to",fixes,"out of",len(records)                

    if fixes > 0:
        fixedRecords = applyGrouping(records = fixedRecords)
        
    return fixedRecords



def applyManualEdit(hashvals, key, value, debug = True):
    if isinstance(value, str):
        value = unicode(value, 'utf-8')
        
    if not isinstance(hashvals, list):
        hashvals = [hashvals]
        
    nfixes = 0
    fixed = getEdits()
    for hashval in hashvals:
        if fixed.get(hashval):
            if key in fixed[hashval].keys():
                beforeVal = fixed[hashval][key]
                if beforeVal == None:
                    fixed[hashval][key] = value
                    nfixes += 1
                elif type(beforeVal) == type(value):
                    fixed[hashval][key] = value
                    nfixes += 1
                else:
                    if debug:
                        print "Did not set",key,"to",value,"because the type",
                        print "(",type(beforeVal),"vs",type(value),") isn't correct."
            else:
                if debug:
                    print "Did not set",key,"to",value,"because that key doesn't exist."
        else:
            if debug:
                print "Did not set",key,"to",value,"hash does not exist."

    if nfixes > 0:
        if debug:
            print "Set",key,"to",value,"for",hashval
        saveEdits(fixed)
    
    

###############################################################################
#
# Check Edits
#
###############################################################################
def checkEdits():
    print "Checking Edit File for Consistency"
    fixed = getEdits()
    for hashval,record in fixed.iteritems():
        checkHash(rec=record, hashval=hashval, quitOnError=True)


###############################################################################
#
# Common Known Edits
#
###############################################################################
def applyCommonKnownEdits(basedir = "/Users/tgadfort/Documents/finance", 
                          debug = False):
    fixed = getEdits()
    data  = getData() 
    
    for hashval in fixed.keys():
        v = fixed[hashval]
        cat   = v['category']
        payee = v['payee']
        fixed[hashval]['transfer'] = v['transfer'].upper()
        fixed[hashval]['date'] = getDate(v['date'])

        if data.get(hashval):
            fixed[hashval] = data[hashval]
            fixed[hashval]['date'] = getDate(fixed[hashval]['date'])        



        newpayee = None
        newcat   = None
        if v['account'] == "Chase":
            if cat == "CreditCardPayment":
                newpayee = "ChaseCreditCard"
        if v['account'] == "Credit":
            if cat == "CreditCardPayment":
                newcat = "CreditCardReceipt"
                cat = newcat
            if cat == "CreditCardReceipt":
                newpayee = "ChaseBankAccount"
            if payee == "ChaseCreditCardPayment":
                newpayee = "ChaseBankAccount"
            if payee == "Chase":
                newpayee = "ChaseBankAccount"


        if newpayee:
            fixed[hashval]['payee'] = newpayee
        if newcat:
            fixed[hashval]['category'] = newcat


        payee = fixed[hashval]['payee']
        cat   = fixed[hashval]['category']
        

        newcat = None
        if cat == "VanguardThomasContributionPayment":
            newcat="VanguardRetirementThomasContributionPayment"
            newpayee="VanguardRetirementThomas"
        if cat == "VanguardRuthContributionPayment":
            newcat="VanguardRetirementRuthContributionPayment"
            newpayee="VanguardRetirementRuth"
            
        if newcat:
            fixed[hashval]['category'] = newcat
        if newpayee:
            fixed[hashval]['payee'] = newpayee
                   
        payee = fixed[hashval]['payee']
        cat   = fixed[hashval]['category']

        if cat == "TIAAFNALThomasConversion":
            fixed[hashval]['transfer'] = "NONE"
         
            
    fixed = applyGrouping(records = fixed)
    saveEdits(fixed)
            
    
def applySpecificKnownEdits(basedir = "/Users/tgadfort/Documents/finance", 
                          debug = False):
    fixed  = getEdits()
    nfixes = 0
    for hashval,record in fixed.iteritems():
        account  = record['account']
        payee    = record['payee']
        category = record['category']
        typeval  = record['type']
        original = record['original']
        date     = record['date']
        midgroup = record['midgroup']

        if category == "Mortgage":
            if payee.find("HomeMortgage") != -1:
                record['payee'] == "WellsFargoLoan"
                nfixes += 1

        continue
    
        if payee == "Metra":
            record['category'] = "NormalTravel"

        if category == "WorkFermilabReimbursement" and payee == "Fermilab":
            record['payee'] = "FermilabTravel"
            nfixes += 1
        if category == "WorkBNLReimbursement" and payee == "BNL":
            record['payee'] = "BNLTravel"
            nfixes += 1

        if midgroup == "Work":
            if matchDateRange(date, ['11/1/2012', '11/4/2012']):
                record['comment'] = "Ruth Trip to Seattle"
                nfixes += 1
            if matchDateRange(date, ['3/20/2013', '4/4/2013']):
                record['comment'] = "Another Ruth Trip to Seattle"
                nfixes += 1
            if matchDateRange(date, ['5/28/2013', '5/28/2013']):
                record['comment'] = "Another Ruth Trip to Seattle"
                nfixes += 1
            if matchDateRange(date, ['6/15/2013', '7/1/2013']):
                record['comment'] = "Ruth Work Trip to Salt Lake City"
                nfixes += 1
            if matchDateRange(date, ['9/27/2015', '10/4/2015']):
                record['comment'] = "Ruth Work Trip to Seattle"
                nfixes += 1
            if matchDateRange(date, ['4/9/2015', '4/11/2015']):
                record['comment'] = "Ruth Trip to Williamsburg"
                nfixes += 1
            if matchDateRange(date, ['6/15/2015', '6/15/2015']):
                record['comment'] = "Ruth Trip to Williamsburg"
                nfixes += 1
            if matchDateRange(date, ['4/20/2015', '4/24/2015']):
                record['comment'] = "Ruth Trip to Los Alamos"
                nfixes += 1
            if matchDateRange(date, ['5/4/2015', '5/4/2015']):
                record['comment'] = "Ruth Trip to Los Alamos"
                nfixes += 1
            if matchDateRange(date, ['5/14/2015', '5/22/2015']):
                record['comment'] = "Ruth Trip to Vail, CO"
                nfixes += 1
            if matchDateRange(date, ['6/18/2015', '6/18/2015']):
                record['comment'] = "Ruth Trip to Vail, CO"
                nfixes += 1
            if matchDateRange(date, ['11/15/2015', '11/23/2015']):
                record['comment'] = "Ruth Trip to Gaithersburg"
                nfixes += 1
            if matchDateRange(date, ['1/20/2016', '1/20/2016']):
                record['comment'] = "Ruth Trip to Gaithersburg"
                nfixes += 1
            if matchDateRange(date, ['11/9/2015', '11/9/2015']):
                record['comment']  = "PRD Editors Pay"
                record['payee']    = "PRD"
                record['category'] = "RuthSalaryExtra"
                nfixes += 1
            if matchDateRange(date, ['6/30/2017', '7/3/2017']):
                record['comment'] = "Sara and Peter's Wedding"
                record['category'] = 'Vacation'
                nfixes += 1
            if matchDateRange(date, ['6/17/2017', '6/19/2017']):
                record['comment']  = "Ruth Trip to Spain"
                record['category'] = 'WorkFermilabSpending'
                nfixes += 1
            if matchDateRange(date, ['5/10/2017', '5/10/2017']):
                record['comment']  = "Ruth Trip to Spain"
                record['category'] = 'WorkFermilabSpending'
                nfixes += 1
            if matchDateRange(date, ['6/5/2017', '6/5/2017']) and payee == "GranadaTravel":
                record['comment']  = "Ruth Trip to Spain"
                record['category'] = 'WorkFermilabSpending'
                nfixes += 1
            if matchDateRange(date, ['6/5/2017', '6/5/2017']) and payee != "GranadaTravel":
                record['comment']  = "Ruth Conference in St. Charles"
                record['category'] = 'WorkFermilabSpending'
                nfixes += 1
            if matchDateRange(date, ['3/2/2017', '3/2/2017']):
                record['comment']  = "Thomas Trip to Rosemont"
                record['category'] = 'WorkAnthemReimbursement'
                nfixes += 1
            if matchDateRange(date, ['8/03/2016', '8/11/2016']):
                record['comment'] = "Ruth ICHEP in Chicago Trip"
                record['category'] = 'WorkFermilabSpending'
                nfixes += 1
            if matchDateRange(date, ['10/10/2016', '11/25/2016']):
                record['comment'] = "Ruth CERN Trip"
                record['category'] = 'WorkFermilabSpending'
                nfixes += 1
            if matchDateRange(date, ['12/03/2016', '12/11/2016']):
                record['comment'] = "Ruth INT Trip"
                record['category'] = 'WorkFermilabSpending'
                nfixes += 1
            if matchDateRange(date, ['7/5/2017', '7/5/2017']):
                record['comment'] = "Last Cape Cod Trip"
                record['category'] = 'Vacation'
                nfixes += 1
            if matchDateRange(date, ['7/20/2017', '7/20/2017']):
                record['comment'] = "Last Cape Cod Trip"
                record['category'] = 'Vacation'
                nfixes += 1
            if matchDateRange(date, ['4/22/2015', '4/28/2015']) and category == "WorkOpenDataSpending":
                record['comment'] = "Thomas Trip to Bridgewater Ass"
                nfixes += 1
            if matchDateRange(date, ['9/28/2014', '9/30/2014']):
                record['comment'] = "Ruth Trip to MIT"
                nfixes += 1
            if matchDateRange(date, ['10/23/2014', '10/23/2014']):
                record['comment'] = "Ruth Trip to MIT"
                nfixes += 1
            if matchDateRange(date, ['7/27/2014', '7/30/2014']):
                record['comment'] = "Ruth Work Trip to Rockville"
                nfixes += 1
            if matchDateRange(date, ['8/15/2014', '8/15/2014']):
                record['comment'] = "Ruth Work Trip to Rockville"
                nfixes += 1
            if matchDateRange(date, ['5/4/2014', '5/6/2014']):
                record['comment'] = "Ruth Trip to Pitt"
                nfixes += 1
            if matchDateRange(date, ['4/14/2014', '4/18/2014']):
                record['comment'] = "Ruth Work Trip to JLab"
                nfixes += 1
            if matchDateRange(date, ['1/7/2014', '1/27/2014']):
                record['comment'] = "Thomas Trip to Aspen"
                nfixes += 1
            if matchDateRange(date, ['12/30/2013', '12/30/2013']):
                record['comment'] = "Thomas Trip to Aspen"
                nfixes += 1
            if matchDateRange(date, ['7/28/2013', '8/2/2013']):
                record['comment'] = "Ruth Trip to Snowmass in Minnesota"
                nfixes += 1
            if matchDateRange(date, ['4/29/2013', '4/29/2013']):
                record['comment'] = "Ruth Work Trip to UoIowa"
                nfixes += 1
            if matchDateRange(date, ['3/10/2013', '3/12/2013']):
                record['comment'] = "Ruth Work Trip to UoIowa"
                nfixes += 1
            if matchDateRange(date, ['5/8/2013', '5/11/2013']):
                record['comment'] = "Ruth Trip to JLab"
                nfixes += 1
            if matchDateRange(date, ['6/10/2013', '6/10/2013']):
                record['comment'] = "Ruth Trip to JLab"
                nfixes += 1
            if matchDateRange(date, ['8/22/2012', '8/22/2012']):
                record['comment'] = "Ruth Work Trip to Cincinnati"
                nfixes += 1
            if matchDateRange(date, ['12/24/2012', '12/24/2012']):
                record['comment'] = "Ruth Work Trip to Cincinnati"
                nfixes += 1
            if matchDateRange(date, ['9/28/2012', '10/2/2012']):
                record['comment'] = "Ruth Work Trip to Cincinnati"
                nfixes += 1
            if matchDateRange(date, ['4/6/2016', '4/6/2016']):
                record['comment'] = "Ruth Work Trip to Wayne State"
                nfixes += 1
            if matchDateRange(date, ['10/23/2015', '10/23/2015']):
                record['comment'] = "Ruth Work Trip to Wayne State"
                nfixes += 1
            if matchDateRange(date, ['12/8/2015', '12/8/2015']):
                record['comment'] = "Ruth Trip to Notre Dame"
                nfixes += 1
            if matchDateRange(date, ['1/19/2016', '1/19/2016']):
                record['comment'] = "Ruth Trip to Notre Dame"
                nfixes += 1
            if matchDateRange(date, ['6/5/2016', '6/9/2016']):
                record['comment'] = "Ruth Work Trip to CalTech"
                nfixes += 1
            if matchDateRange(date, ['7/26/2015', '7/31/2015']):
                record['comment'] = "Ruth Trip to Vienna"
                nfixes += 1
            if matchDateRange(date, ['9/8/2015', '9/11/2015']):
                record['comment'] = "Ruth Trip to Vienna"
                nfixes += 1
            if matchDateRange(date, ['6/4/2015', '6/4/2015']) and payee == "VbrAustropa":
                record['comment'] = "Ruth Trip to Vienna"
                nfixes += 1
            if matchDateRange(date, ['1/20/2009', '1/20/2009']):
                record['comment'] = "Ruth Work Trip to Tuscon"
                nfixes += 1
            if matchDateRange(date, ['3/24/2009', '3/24/2009']):
                record['comment'] = "Ruth Work Trip to Tuscon"
                nfixes += 1
            if matchDateRange(date, ['1/29/2009', '2/6/2009']):
                record['comment'] = "Ruth Trip to Ohio State"
                nfixes += 1
            if matchDateRange(date, ['3/13/2009', '3/13/2009']):
                record['comment'] = "Ruth Trip to Ohio State"
                nfixes += 1
            if matchDateRange(date, ['3/16/2009', '3/18/2009']):
                record['comment'] = "Joint Trip to Moriond"
                nfixes += 1
            if matchDateRange(date, ['7/9/2009', '7/14/2009']):
                record['comment'] = "Thomas Trip to SLAC"
                nfixes += 1
            if matchDateRange(date, ['9/4/2009', '9/4/2009']):
                record['comment'] = "Ruth Trip to Lake Placid"
                nfixes += 1
            if matchDateRange(date, ['10/4/2009', '10/4/2009']):
                record['comment'] = "Thomas Work Trip to Boston"
                nfixes += 1
            if matchDateRange(date, ['10/28/2009', '11/1/2009']):
                record['comment'] = "Ruth BNL Trip to SLAC"
                nfixes += 1
            if matchDateRange(date, ['11/29/2009', '12/10/2009']):
                record['comment'] = "Thomas Work Trip to CERN"
                nfixes += 1
            if matchDateRange(date, ['4/26/2010', '4/26/2010']):
                record['comment'] = "Thomas Trip to Oxford"
                nfixes += 1
            if matchDateRange(date, ['4/28/2010', '5/5/2010']):
                record['comment'] = "Thomas Trip to CERN"
                nfixes += 1
            if matchDateRange(date, ['6/20/2010', '6/25/2010']):
                record['comment'] = "Thomas Trip to Oxford"
                nfixes += 1
            if matchDateRange(date, ['8/19/2010', '8/25/2010']):
                record['comment'] = "Thomas Work Trip to SLAC"
                nfixes += 1
            if matchDateRange(date, ['12/7/2010', '12/8/2010']):
                record['comment'] = "Ruth Trip to Yale"
                nfixes += 1
            if matchDateRange(date, ['1/10/2011', '1/10/2011']):
                record['comment'] = "Ruth Trip to Yale"
                nfixes += 1
            if matchDateRange(date, ['1/15/2011', '1/18/2011']):
                record['comment'] = "Ruth Work Trip to Tuscon"
                nfixes += 1
            if matchDateRange(date, ['2/3/2011', '2/4/2011']):
                record['comment'] = "Thomas Interview at FNAL"
                nfixes += 1
            if matchDateRange(date, ['3/2/2011', '3/2/2011']):
                record['comment'] = "Thomas Trip to CERN and Pisa"
                nfixes += 1
            if matchDateRange(date, ['4/13/2011', '4/21/2011']):
                record['comment'] = "Thomas Trip to CERN and Pisa"
                nfixes += 1
            if matchDateRange(date, ['3/15/2011', '3/15/2011']):
                record['comment'] = "Ruth Trip to Germantown"
                nfixes += 1
            if matchDateRange(date, ['5/6/2011', '5/7/2011']):
                record['comment'] = "Ruth Work Trip to JLab"
                nfixes += 1
            if matchDateRange(date, ['6/19/2011', '6/25/2011']):
                record['comment'] = "Ruth Trip to Salt Lake City"
                nfixes += 1
            if matchDateRange(date, ['7/11/2011', '7/17/2011']):
                record['comment'] = "Ruth Work to Lake Tahoe"
                nfixes += 1
            if matchDateRange(date, ['5/29/2011', '5/29/2011']):
                record['comment'] = "Ruth Work to Lake Tahoe"
                nfixes += 1
            if matchDateRange(date, ['8/13/2011', '8/18/2011']):
                record['comment'] = "Thomas ATLAS Trip to Boston"
                nfixes += 1
            if matchDateRange(date, ['9/7/2011', '9/10/2011']):
                record['comment'] = "Ruth Interview at FNAL"
                nfixes += 1
            if matchDateRange(date, ['10/4/2011', '10/4/2011']):
                record['comment'] = "Ruth Trip to Kyoto"
                nfixes += 1
            if matchDateRange(date, ['10/31/2011', '12/4/2011']):
                record['comment'] = "Joint Trip to Intensity Frontier"
                nfixes += 1
            if matchDateRange(date, ['12/22/2011', '12/29/2011']):
                record['comment'] = "Joint Trip to Intensity Frontier"
                nfixes += 1
            if matchDateRange(date, ['1/13/2012', '1/17/2012']):
                record['comment'] = "Ruth 2012 Trip to Tuscon"
                nfixes += 1
            if matchDateRange(date, ['1/27/2012', '1/27/2012']):
                record['comment'] = "Ruth 2012 Trip to Tuscon"
                nfixes += 1
            if matchDateRange(date, ['1/26/2012', '1/26/2012']):
                record['comment'] = "Ruth Trip to Aspen"
                nfixes += 1
            if matchDateRange(date, ['1/30/2012', '1/30/2012']):
                record['comment'] = "Ruth Trip to Aspen"
                nfixes += 1
            if matchDateRange(date, ['2/11/2012', '2/18/2012']):
                record['comment'] = "Ruth Trip to Aspen"
                nfixes += 1
            if matchDateRange(date, ['3/2/2012', '3/2/2012']):
                record['comment'] = "Ruth Trip to Aspen"
                nfixes += 1
            if matchDateRange(date, ['1/19/2012', '1/21/2012']):
                record['comment'] = "Thomas Staff Interview at FNAL"
                nfixes += 1
            if matchDateRange(date, ['2/6/2012', '2/11/2012']):
                record['comment'] = "Ruth Trip to UW"
                nfixes += 1
            if matchDateRange(date, ['3/19/2012', '3/19/2012']):
                record['comment'] = "Ruth Trip to UW"
                nfixes += 1
            if matchDateRange(date, ['4/16/2012', '4/20/2012']):
                record['comment'] = "Thomas Trip to Ann Arbor"
                nfixes += 1
            if matchDateRange(date, ['5/25/2012', '5/31/2012']):
                record['comment'] = "Ruth Trip to Tampa"
                nfixes += 1
            if matchDateRange(date, ['6/27/2012', '6/30/2012']):
                record['comment'] = "Thomas g-2 Trip to FNAL"
                nfixes += 1
            if matchDateRange(date, ['7/19/2012', '7/19/2012']):
                record['comment'] = "Thomas g-2 Trip to FNAL"
                nfixes += 1
            if matchDateRange(date, ['7/23/2012', '7/23/2012']):
                record['comment'] = "Thomas ATLAS Trip to Michigan"
                nfixes += 1
            if matchDateRange(date, ['8/12/2012', '8/16/2012']):
                record['comment'] = "Thomas ATLAS Trip to Michigan"
                nfixes += 1
            if matchDateRange(date, ['7/25/2012', '7/27/2012']):
                record['comment'] = "Ruth Group Trip to Rockville"
                nfixes += 1
            if matchDateRange(date, ['6/13/2012', '6/20/2012']):
                record['comment'] = "Ruth Project X Trip to FNAL"
                nfixes += 1
            
        if category == "Vacation":
            if matchDateRange(date, ['7/1/2009', '7/10/2009']):
                record['comment'] = "Some Summer Vacation"
                nfixes += 1
            if matchDateRange(date, ['8/24/2009', '8/25/2009']):
                record['comment'] = "Chris's Bachelor Party in Montreal"
                nfixes += 1
            if matchDateRange(date, ['9/1/2009', '9/9/2009']):
                record['comment'] = "Chris's Wedding in Maine"
                nfixes += 1
            if matchDateRange(date, ['9/22/2009', '9/30/2009']):
                record['comment'] = "Babymoon in San Diego"
                nfixes += 1
            if matchDateRange(date, ['11/27/2009', '11/27/2009']):
                record['comment'] = "Pre-Claire Thanksgiving in NJ"
                nfixes += 1
            if matchDateRange(date, ['4/15/2010', '5/23/2010']):
                record['comment'] = "David's Wedding in South Carolina"
                nfixes += 1
            if matchDateRange(date, ['7/31/2010', '8/7/2010']):
                record['comment'] = "Claire's First Trip to Cape Cod"
                nfixes += 1
            if matchDateRange(date, ['10/22/2010', '10/26/2010']):
                record['comment'] = "Thanksgiving in NJ"
                nfixes += 1
            if matchDateRange(date, ['8/20/2011', '8/26/2011']):
                record['comment'] = "Post Hearing-Aid Trip to Cape Cod"
                nfixes += 1
            if matchDateRange(date, ['3/2/2012', '3/6/2012']):
                record['comment'] = "Visiting Boston"
                nfixes += 1
            if matchDateRange(date, ['7/4/2012', '7/14/2012']):
                record['comment'] = "Last Long Island to Cape Cod Trip"
                nfixes += 1
            if matchDateRange(date, ['8/29/2012', '9/6/2012']):
                record['comment'] = "Moving to Illinois"
                nfixes += 1
            if matchDateRange(date, ['7/13/2013', '7/23/2013']):
                record['comment'] = "Driving Trip to Cape Cod"
                nfixes += 1
            if matchDateRange(date, ['8/16/2013', '8/20/2013']):
                record['comment'] = "First Summer Trip to Knoxville"
                nfixes += 1
            if matchDateRange(date, ['11/25/2014', '11/29/2014']):
                record['comment'] = "Thanksgiving in Knoxville"
                nfixes += 1
            if matchDateRange(date, ['6/19/2015', '6/26/2015']):
                record['comment'] = "Thanksgiving in Knoxville"
                nfixes += 1
            if matchDateRange(date, ['6/27/2016', '7/11/2016']):
                record['comment'] = "Two Week Knoxville Trip"
                nfixes += 1
            if matchDateRange(date, ['8/1/2016', '9/12/2016']):
                record['comment'] = "Battle at Bristol"
                nfixes += 1
            if matchDateRange(date, ['4/11/2017', '5/16/2017']):
                record['comment'] = "Emma's Wedding"
                nfixes += 1
            if matchDateRange(date, ['5/26/2017', '7/3/2017']):
                record['comment'] = "Sara and Peter's Wedding"
                nfixes += 1
            if matchDateRange(date, ['7/7/2017', '7/15/2017']):
                record['comment'] = "Last Cape Cod Trip"
                nfixes += 1
            if matchDateRange(date, ['7/25/2017', '7/25/2017']):
                record['comment'] = "Last Cape Cod Trip"
                nfixes += 1
            if matchDateRange(date, ['7/29/2017', '7/29/2017']):
                record['comment'] = "My Madison Trip"
                nfixes += 1
        
        if False:
            print hashval,account,payee,category,typeval,original
    
    print "Fixed",nfixes,"edit records."
    if nfixes > 0:
        fixed = applyGrouping(records = fixed)
        saveEdits(fixed)
    else:
        print "Just returning without edit."


###############################################################################
#
# Some records are intentionally set to Todo for their category so fix it.
#
###############################################################################
def fixTodo(basedir = "/Users/tgadfort/Documents/finance", account = "Portfolio",
            debug = False, stop = False):
    fixed = getEdits()
    data  = getData()
    recs  = findRecords(cutCategory="Todo")
    print "Found",len(recs),"Todo entries."
    
    vanguard = findRecords(cutAccount=["VanguardRetirementRuth", "VanguardRetirementThomas", "VanguardTradIRARuth", "VanguardTradIRAThomas"])
    college  = findRecords(cutAccount=["Charlie529IL", "Claire529IL", "Claire529NY"])
    
    tfmap = {}
    for hashval,v in recs.iteritems():
        minDays = 11
        minHash = None
        if v['payee'] == "IRAPayment":
            for matchHash,matchRecord in vanguard.iteritems():
                if tfmap.get(matchHash): continue
                days = countDays(v['date'], matchRecord['date'], makeABS=True)
                if days < minDays:
                    minHash = matchHash
                    minDays = days

        if v['payee'] == "529Payment":
            for matchHash,matchRecord in college.iteritems():
                if tfmap.get(matchHash): continue
                days = countDays(v['date'], matchRecord['date'], makeABS=True)
                if days < minDays:
                    minHash = matchHash
                    minDays = days
        
        
        print v['date'],'\t',v['account'],'\t',v['category'],'\t',v['payee'],'\t\t',v['amount'],'\t',
        if minHash:
            rec = data[minHash]
            print rec['account'],'\t',minDays
            if fixed.get(hashval) == None:
                fixed[hashval] = v
            if rec['account'] == "VanguardRetirementThomas": 
                fixed[hashval]['category'] = "VanguardRetirementThomasContributionPayment"
                fixed[hashval]['payee']    = rec['account']
            if rec['account'] == "VanguardRetirementRuth": 
                fixed[hashval]['category'] = "VanguardRetirementRuthContributionPayment"
                fixed[hashval]['payee']    = rec['account']
            if rec['account'] == "VanguardTradIRAThomas":
                fixed[hashval]['category'] = "VanguardTradIRAThomasContributionPayment"
                fixed[hashval]['payee']    = rec['account']
            if rec['account'] == "VanguardTradIRARuth":
                fixed[hashval]['category'] = "VanguardTradIRARuthContributionPayment"
                fixed[hashval]['payee']    = rec['account']
            if rec['account'] == "Charlie529IL": 
                fixed[hashval]['category'] = "Charlie529ILPayment"
                fixed[hashval]['payee']    = rec['account']
            if rec['account'] == "Claire529IL": 
                fixed[hashval]['category'] = "Claire529ILPayment"
                fixed[hashval]['payee']    = rec['account']
            if rec['account'] == "Claire529NY": 
                fixed[hashval]['category'] = "Claire529NYPayment"
                fixed[hashval]['payee']    = rec['account']
            tfmap[minHash] = hashval
        else:
            print "NO MATCH!!!"

    print ""
    print "Fixed",len(tfmap),"Todos."
    print ""

    if len(tfmap) > 0:
        fixed = applyGrouping(records = fixed)
        saveEdits(fixed)
    else:
        print "Just returning without edit."
    
    
    
###############################################################################
#
# Clean data for final writing
#
###############################################################################
def reverseCredit(records):
    if isinstance(records, list):
        for record in records:
            if record['account'] == 'Credit':
                record['amount'] *= -1
        return records
    elif isinstance(records, dict):
        for hashval,record in records.iteritems():
            if record['account'] == 'Credit':
                record['amount'] *= -1
        return records
    else:
        raise ValueError("Could not understand records in reverseCredit")






###############################################################################
#
# Produce inverted maps for payees/categories
#
###############################################################################
def createCorrectedPayeeMapping(basedir = "/Users/tgadfort/Documents/finance",):
    
    print "Loading corrected payee data."
    filename = setSubFile(getLocation(), "schema", "CorrPayeeData.yaml")
    corrdata = get(filename)
    
    print "Loading group mapping."
    categoryMap = getGroupMapping()

    correctedData = {}    
    payeeData={}
    payeeCategoryData={}
    categoryPayeeData={}
    categoryData={}
    for category in categoryMap.keys(): categoryData[category] = 1

    for payee,payeedata in corrdata.iteritems():
        payeeData[payee] = 1
        corrections = payeedata.get('corrections')
        if corrections == None:
            raise ValueError("Payee",payee,"doesn't have a corrections key")

        category = payeedata.get('category')
        if category == None:
            raise ValueError("Payee",payee,"doesn't have a category")

        if categoryMap.get(category) == None:
            raise ValueError("Category:",category,"for payee:",payee,"not in approved list.")

        payeeCategoryData[payee] = category
        if categoryPayeeData.get(category) == None:
            categoryPayeeData[category] = {}
        categoryPayeeData[category][payee] = 1

        for correction in corrections:
            if correctedData.get(correction):
                raise ValueError("Double corrections (",correction,") for",payee,"and",correctedData[correction])
            correctedData[correction] = payee
        
    save(setFile(getLocation(),"payeeCorrections.json"), correctedData, debug=True)
    save(setFile(getLocation(),"payees.json"), payeeData, debug=True)
    save(setFile(getLocation(),"categories.json"), categoryData, debug=True)
    save(setFile(getLocation(),"payeeCategory.json"), payeeCategoryData, debug=True)
    save(setFile(getLocation(),"categoryPayee.json"), categoryPayeeData, debug=True)
    save(setSubFile(getLocation(),"schema", "CorrPayeeData.yaml"), corrdata, debug=True)



###############################################################################
#
# Check for known payee/category
#
###############################################################################
def getCorrPayee(basepayee, debug = False):
    correctedData = get(setFile(getLocation(),"payeeCorrections.json"))
    for corrTestValue,corrPayee in correctedData.iteritems():
        if basepayee.find(corrTestValue) != -1:
            if debug:
                print "Returning",corrPayee,"as payee based on",basepayee
            return corrPayee
    
    if debug:
        print "Did not find a known correction for",basepayee

    return basepayee


def getPayeeCategory(payee, debug = False):
    payeeCategoryData = get(setFile(getLocation(),"payeeCategory.json"))
    
    payeeCategory = payeeCategoryData.get(payee)
    if payeeCategory == None and debug:
        print "No known category for payee:",payee

    if debug:
        print "Returning",payeeCategory,"as category for payee",payee
        
    return payeeCategory


def isKnownPayee(payee, debug = False):
    payeeFile = setFile(getLocation(),"payees.json")
    payeeData = get(payeeFile)
    if payeeData.get(payee) == None:
        if debug:
            print "Payee",payee,"is not known."
        return False
    return True


def isKnownCategory(category, debug = False):
    categoryData = get(setFile(getLocation(),"categories.json"))
    if categoryData.get(category) == None:
        if debug:
            print "Category",category,"is not known."
        return False
    return True


def checkPayee(payee, row = None, cleanpayee = None, quitOnError = True):
    if not isKnownPayee(payee):
        if row:
            print "Original Payee -->",row["Payee"]
        print "Clean Payee    -->",cleanpayee
        print "Row[]          -->",row
        if quitOnError:
            raise ValueError("Payee -->",payee,"is not known...")
        return False
    return True


def checkCategory(category, row = None, payee = None, cleanpayee = None, quitOnError = True):
    if not isKnownCategory(category):
        if row:
            print "Original Payee -->",row["Payee"]
        print "Clean Payee    -->",cleanpayee
        print "Final Payee    -->",payee
        print "Row[]          -->",row
        if quitOnError:
            raise ValueError("Category -->",category,"is not known...")
        return False
    return True