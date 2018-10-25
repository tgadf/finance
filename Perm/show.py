# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 10:39:07 2015

@author: tgadfort
"""

import sys
import csv
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

from strops import nice
from strdate import writeDate, getDate, countMonths, getMinMaxStatements
from getter import sortRecords, getData, formatRecords
from collections import Counter
from recsearch import findRecords
from uid import getHash


##########################################
#
# Helper
#
##########################################
def clean(val):
    val = int(100*val)
    return float(float(val)/100.0)





###############################################################################
#
# Print Record
#
###############################################################################
def printRecords(records, sort = False, showHash = False, skipAmount = False, 
                 skipComment = False, skipOriginal = False, fixdir = False, f = None):
    allRecords = formatRecords(records, sort=sort, returnList=True)

    # get max amount/running amt

    sumval=0
    for i,record in enumerate(allRecords):
        if i % 5000 == 0:
            vals1=[nice("#", 4),nice("Date",12),nice("Payee", 20),nice("Category", 20),nice("Account", 15)]
            vals2=[nice('-', 4),nice('----',12),nice('-----', 20),nice('--------', 20),nice('-------', 15)]
            if not skipAmount:
                vals1 += [nice("Amount", 10),nice("Running Sum", 12)]
                vals2 += [nice("------", 10),nice("-----------", 12)]
            vals1 += [nice("R",3)]
            vals2 += [nice("-",3)]
            if not skipComment:
                vals1 += [nice("Comment",20)]
                vals2 += [nice("-------",20)]
            if not skipOriginal:
                if showHash:
                    vals1 += [nice("Hash",40)]
                    vals2 += [nice("----",40)]
                else:
                    vals1 += [nice("Original",40)]
                    vals2 += [nice("--------",40)]
                        
            if f:
                f.write("\n")
                f.write("".join(vals1)+"\n")
                f.write("".join(vals2)+"\n\n")
            else:
                print ""
                print "".join(vals1)
                print "".join(vals2)
                
        sumval = printRecord(i, sumval, record, showHash, skipAmount, skipComment, skipOriginal, f)
    print ''



###############################################################################
#
# Print
#
###############################################################################    
def printRecord(num, sumval, record, showHash, skipAmount, skipComment, skipOriginal, f = None):
    try:
        int(record['key'])
    except:
        print record
        exit()

    try:
        saving=record['saving']
        transfer=record['transfer']
        ignore=record['ignore']
    except:
        saving=None
        transfer=None
        ignore=None

    try:
        comment=record['comment']
    except:
        comment="-"

    try:
        reconciled=record['reconciled']
    except:
        reconciled="-"
    if reconciled == None:
        reconciled="-"
    elif reconciled == "None":
        reconciled="-"
    if reconciled == "No":
        reconciled="N"
    if reconciled == "Yes":
        reconciled="Y"
            

    if transfer == None:
        transfer=" - "
    elif transfer == 1:
        transfer="<- "
    elif transfer == -1:
        transfer=" ->"

    if saving == None:
        saving=" - "
    elif saving == 1:
        saving="<- "
        
    if ignore == True:
        ignore=" T "
    elif ignore == False:
        ignore=" F "
    else:
        ignore=" F "

    if ignore == " F ":
        sf = 1
        if record['account'] == "Credit":
            sf = -1
        sumval += sf*float(record['amount'])
 
    original = record.get('original')
    if original == None:
        original = "-"
    
    # old
    ## print nice(num, 4),nice(writeDate(record['date']),15), nice(record['payee'], 40),nice(record['category'],35),nice(record['account'],15),nice(transfer,5),nice(saving,5),nice(ignore,5),nice(record['amount'], 10),nice(sumval, 12),nice(key,3),nice(uid.getHash(record)[key],35)

    # new
    amt = float(record['amount'])
    if amt >= 0:
        amt = "+"+str(amt)
    else:
        amt = str(amt)

    vals = [nice(num, 4),nice(writeDate(record['date']),12), nice(record['payee'], 19)," ",
            nice(record['category'],19)," ",nice(record['account'],15)]
    if not skipAmount:
        vals += [nice(amt, 10),nice(sumval, 12)]
    
    vals += [nice(reconciled,3)]
    if not skipComment:
        vals += [nice(comment,19)," "]
    
    if not skipOriginal:
        if showHash:
            hashval = getHash(record)
            vals += [nice(hashval,40)]
        else:
            vals += [nice(original,40)]
            
    if f:
        f.write("".join(vals)+"\n")
    else:
        print "".join(vals)
        
    return sumval




###############################################################################
#
# Profiles
#
############################################################################### 
def profilePayees(payee = None, showResults = False, showRecords = False):
    data = getData()
    sums, yearlysums = computeSummary(data)
    payees = sorted(sums['payees'].keys())
    if payee:
        payees = [payee]
    for k in payees:
        v = sums['payees'][k]
        print "  ",nice(k, 50),'\t',nice(int(sums['counts'][k]), 10),'\t',clean(v)
        if showRecords:            
            records = findRecords(cutPayee = k, sort=True)
            printRecords(records)
            

###############################################################################
#
# Summary
#
############################################################################### 
def showDiffCategorySummary(data1, data2, show=True):
    sums1={}
    sums2={}
    sums1['categories']=Counter()
    sums1['counts']=Counter()
    sums2['categories']=Counter()
    sums2['counts']=Counter()

    if isinstance(data1, dict):
        for k,v in data1.iteritems():
            sums1['categories'][v['category']] += float(v['amount'])
            sums1['counts'][v['category']] += 1
    if isinstance(data2, dict):
        for k,v in data2.iteritems():
            sums2['categories'][v['category']] += float(v['amount'])
            sums2['counts'][v['category']] += 1
    if isinstance(data1, list):
        for v in data1:
            sums1['categories'][v['category']] += float(v['amount'])
            sums1['counts'][v['category']] += 1
    if isinstance(data1, list):
        for v in data2:
            sums2['categories'][v['category']] += float(v['amount'])
            sums2['counts'][v['category']] += 1


    print "Diffs:"
    for k,v in sums1['categories'].iteritems():
        diff = sums1['counts'][k] - sums2['counts'][k]
        if abs(diff) > 0:
            print "  ",nice(k, 50),'\t',nice(diff, 10),'\t',v

    for k,v in sums2['categories'].iteritems():
        diff = sums2['counts'][k] - sums1['counts'][k]
        if abs(diff) > 0:
            print "  ",nice(k, 50),'\t',nice(diff, 10),'\t',v


def getCategorySummary(startdate, enddate, account, data, show=True):
    sums={}
    sums['categories']=Counter()
    sums['counts']=Counter()

    if isinstance(data, dict):
        for k,v in data.iteritems():
            sums['categories'][v['category']] += float(v['amount'])
            sums['counts'][v['category']] += 1
    if isinstance(data, list):
        for v in data:
            sums['categories'][v['category']] += float(v['amount'])
            sums['counts'][v['category']] += 1

    if show:
        if startdate != None and enddate != None:
            print "---- Time Info ----"
            print "  ",nice("First Entry", 20),'\t',startdate
            print "  ",nice(" Last Entry", 20),'\t',enddate    
        print "---- Categories ----"
        for k,v in sums['categories'].most_common(1000):
            print "  ",nice(k, 50),'\t',nice(sums['counts'][k], 10),'\t',v
    return sums


def getSumInfo(v):
    retval = {}

    year  = "NoYear"
    if v.get('year'): year = v['year']
    retval["Year"] = year
    
    group = "NoGroup"
    if v.get('group'): group = v['group']
    retval["Group"] = group
    
    midgroup = "NoMidGroup"
    if v.get('midgroup'): midgroup = v['midgroup']
    retval["MidGroup"] = midgroup
    
    supergroup = "NoSuperGroup"
    if v.get('supergroup'): supergroup = v['supergroup']
    retval["SuperGroup"] = supergroup
    
    account = "NoAccount"
    if v.get('account'): account = v['account']
    retval["Account"] = account
    
    vtype = "NoType"
    if v.get('type'): vtype = v['type']
    retval["Type"] = vtype
    
    payee = "NoPayee"
    if v.get('payee'): payee = v['payee']
    retval["Payee"] = payee
    
    transfer = "NoTransfer"
    if v.get('transfer'): transfer = v['transfer']
    retval["Transfer"] = transfer
    
    category = "NoCategory"
    if v.get('category'): category = v['category']
    retval["Category"] = category
    
    return retval


def getSumKeyNames():
    return ['groups', 'supergroups', 'accounts', 'types', 'transfers', 'categories', 'payees']


def getSumKeys(v):
    year,group,supergroup,account,vtype,transfer,category,payee = getSumInfo(v)
    sumkeys  = [group,supergroup,account,vtype,transfer,category,payee]
    return sumkeys


def getSumInfoYear(v): return getSumInfo(v)[0]


def setSums(v, sums):
    sumInfo   = getSumInfo(v)
    amount    = float(v['amount'])
    statement = v['statement']

    ## Correction for Credit Amount
    if v['account'] == "Credit" or v['account'] == "CreditFreedom":
        amount *= -1

    sums['group']['counts'][sumInfo["Group"]] += 1
    sums['group']['amount'][sumInfo["Group"]] += amount
    if sums['group']['months'].get(sumInfo["Group"]) == None:
        sums['group']['months'][sumInfo["Group"]] = {}
    sums['group']['months'][sumInfo["Group"]][statement] = True
        
    sums['midgroup']['counts'][sumInfo["MidGroup"]] += 1
    sums['midgroup']['amount'][sumInfo["MidGroup"]] += amount
    if sums['midgroup']['months'].get(sumInfo["MidGroup"]) == None:
        sums['midgroup']['months'][sumInfo["MidGroup"]] = {}
    sums['midgroup']['months'][sumInfo["MidGroup"]][statement] = True
        
    sums['supergroup']['counts'][sumInfo["SuperGroup"]] += 1
    sums['supergroup']['amount'][sumInfo["SuperGroup"]] += amount
    if sums['supergroup']['months'].get(sumInfo["SuperGroup"]) == None:
        sums['supergroup']['months'][sumInfo["SuperGroup"]] = {}
    sums['supergroup']['months'][sumInfo["SuperGroup"]][statement] = True
        
    sums['account']['counts'][sumInfo["Account"]] += 1
    sums['account']['amount'][sumInfo["Account"]] += amount
    if sums['account']['months'].get(sumInfo["Account"]) == None:
        sums['account']['months'][sumInfo["Account"]] = {}
    sums['account']['months'][sumInfo["Account"]][statement] = True
        
    sums['transfer']['counts'][sumInfo["Transfer"]] += 1
    sums['transfer']['amount'][sumInfo["Transfer"]] += amount
    if sums['transfer']['months'].get(sumInfo["Transfer"]) == None:
        sums['transfer']['months'][sumInfo["Transfer"]] = {}
    sums['transfer']['months'][sumInfo["Transfer"]][statement] = True
        
    sums['category']['counts'][sumInfo["Category"]] += 1
    sums['category']['amount'][sumInfo["Category"]] += amount
    if sums['category']['months'].get(sumInfo["Category"]) == None:
        sums['category']['months'][sumInfo["Category"]] = {}
    sums['category']['months'][sumInfo["Category"]][statement] = True
        
    sums['payee']['counts'][sumInfo["Payee"]] += 1
    sums['payee']['amount'][sumInfo["Payee"]] += amount
    if sums['payee']['months'].get(sumInfo["Payee"]) == None:
        sums['payee']['months'][sumInfo["Payee"]] = {}
    sums['payee']['months'][sumInfo["Payee"]][statement] = True
        
    return sums


def setYearlySums(v, yearlysums):
    sumInfo = getSumInfo(v)
    amount  = float(v['amount'])
    year    = sumInfo["Year"]
    
    if yearlysums['group'].get(sumInfo["Group"]) == None:
        yearlysums['group'][sumInfo["Group"]] = {}
    if yearlysums['group'][sumInfo["Group"]].get(year) == None:
        yearlysums['group'][sumInfo["Group"]][year] = {"counts": Counter(), "amount": Counter()}
    yearlysums['group'][sumInfo["Group"]][year]['counts'] += 1
    yearlysums['group'][sumInfo["Group"]][year]['amount'] += amount
    
    if yearlysums['midgroup'].get(sumInfo["MidGroup"]) == None:
        yearlysums['midgroup'][sumInfo["MidGroup"]] = {}
    if yearlysums['midgroup'][sumInfo["MidGroup"]].get(year) == None:
        yearlysums['midgroup'][sumInfo["MidGroup"]][year] = {"counts": Counter(), "amount": Counter()}
    yearlysums['midgroup'][sumInfo["MidGroup"]][year]['counts'] += 1
    yearlysums['midgroup'][sumInfo["MidGroup"]][year]['amount'] += amount
        
    if yearlysums['supergroup'].get(sumInfo["SuperGroup"]) == None:
        yearlysums['supergroup'][sumInfo["SuperGroup"]] = {}
    if yearlysums['supergroup'][sumInfo["SuperGroup"]].get(year) == None:
        yearlysums['supergroup'][sumInfo["SuperGroup"]][year] = {"counts": Counter(), "amount": Counter()}
    yearlysums['supergroup'][sumInfo["SuperGroup"]][year]['counts'] += 1
    yearlysums['supergroup'][sumInfo["SuperGroup"]][year]['amount'] += amount
        
    if yearlysums['account'].get(sumInfo["Account"]) == None:
        yearlysums['account'][sumInfo["Account"]] = {}
    if yearlysums['account'][sumInfo["Account"]].get(year) == None:
        yearlysums['account'][sumInfo["Account"]][year] = {"counts": Counter(), "amount": Counter()}
    yearlysums['account'][sumInfo["Account"]][year]['counts'] += 1
    yearlysums['account'][sumInfo["Account"]][year]['amount'] += amount
        
    if yearlysums['transfer'].get(sumInfo["Transfer"]) == None:
        yearlysums['transfer'][sumInfo["Transfer"]] = {}
    if yearlysums['transfer'][sumInfo["Transfer"]].get(year) == None:
        yearlysums['transfer'][sumInfo["Transfer"]][year] = {"counts": Counter(), "amount": Counter()}
    yearlysums['transfer'][sumInfo["Transfer"]][year]['counts'] += 1
    yearlysums['transfer'][sumInfo["Transfer"]][year]['amount'] += amount
        
    if yearlysums['category'].get(sumInfo["Category"]) == None:
        yearlysums['category'][sumInfo["Category"]] = {}
    if yearlysums['category'][sumInfo["Category"]].get(year) == None:
        yearlysums['category'][sumInfo["Category"]][year] = {"counts": Counter(), "amount": Counter()}
    yearlysums['category'][sumInfo["Category"]][year]['counts'] += 1
    yearlysums['category'][sumInfo["Category"]][year]['amount'] += amount
        
    if yearlysums['payee'].get(sumInfo["Payee"]) == None:
        yearlysums['payee'][sumInfo["Payee"]] = {}
    if yearlysums['payee'][sumInfo["Payee"]].get(year) == None:
        yearlysums['payee'][sumInfo["Payee"]][year] = {"counts": Counter(), "amount": Counter()}
    yearlysums['payee'][sumInfo["Payee"]][year]['counts'] += 1
    yearlysums['payee'][sumInfo["Payee"]][year]['amount'] += amount
    
        
    return yearlysums


def computeSummary(data):
    #yearlysums = {"group": {}, "midgroup": {}, "supergroup": {}, "account": {}, "transfer": {}, "category": {}, "payee": {}}

    sums               = {}
    sums['group']      = {"counts": Counter(), "amount": Counter(), "months": {}}
    sums['midgroup']   = {"counts": Counter(), "amount": Counter(), "months": {}}
    sums['supergroup'] = {"counts": Counter(), "amount": Counter(), "months": {}}
    sums['account']    = {"counts": Counter(), "amount": Counter(), "months": {}}
    sums['transfer']   = {"counts": Counter(), "amount": Counter(), "months": {}}
    sums['category']   = {"counts": Counter(), "amount": Counter(), "months": {}}
    sums['payee']      = {"counts": Counter(), "amount": Counter(), "months": {}}
            
    if isinstance(data, dict):
        for k,v in data.iteritems():
            sums       = setSums(v, sums)
            #yearlysums = setYearlySums(v, yearlysums)
    elif isinstance(data, list):
        for v in data:
            sums       = setSums(v, sums)
            #yearlysums = setYearlySums(v, yearlysums)
    else:
        raise ValueError("Did not recognize the type of data",type(data))

    return sums #, yearlysums



def getSummary(startdate, enddate, account, data, show=True, showYear=True):
    sums = computeSummary(data)

    if show or showYear:
        if startdate != None and enddate != None:
            print "---- Time Info ----"
            print "  ",nice("First Entry", 20),'\t',startdate
            print "  ",nice(" Last Entry", 20),'\t',enddate    
        print ''
        
        print nice("---- Accounts ----",50),nice("Counts",10),nice("Amount ($)",15)
        key = 'account'
        for name in sorted(sums[key]['counts'].keys()):
            counts = int(sums[key]['counts'][name])
            amount = int(sums[key]['amount'][name])
            print "  ",nice(name, 50),nice(counts,10),nice(amount,15)
        print ''
            
        print nice("---- Payees ----",50),nice("Counts",10),nice("Amount ($)",15)
        key = 'payee'
        for k,v in sums[key]['counts'].most_common(10):
            name   = k
            counts = v
            amount = int(sums[key]['amount'][name])
            print "  ",nice(name, 50),nice(counts,10),nice(amount,15)
        print ''
            
        print nice("---- Categories ----",50),nice("Counts",10),nice("Amount ($)",15)
        key = 'category'
        for k,v in sums[key]['counts'].most_common(20):
            name   = k
            counts = v
            amount = int(sums[key]['amount'][name])
            
            minDate,maxDate = getMinMaxStatements(sums[key]['months'][name].keys())
            months = countMonths(minDate, maxDate)+1
            print "  ",nice(name, 50),nice(counts,10),nice(amount,15),nice(months,4),nice(amount/months,15)
        print ''
        
        print nice("---- Transfers ----",50),nice("Counts",10),nice("Amount ($)",15)
        key = 'transfer'
        for name in sorted(sums[key]['counts'].keys()):
            counts = int(sums[key]['counts'][name])
            amount = int(sums[key]['amount'][name])
            print "  ",nice(name, 50),nice(counts,10),nice(amount,15)
        print ''
        
        print nice("---- Groups ----",50),nice("Counts",10),nice("Amount ($)",15)
        key = 'group'
        for name in sorted(sums[key]['counts'].keys()):
            counts = int(sums[key]['counts'][name])
            amount = int(sums[key]['amount'][name])
            print "  ",nice(name, 50),nice(counts,10),nice(amount,15)
        print ''
        
        print nice("---- MidGroups ----",50),nice("Counts",10),nice("Amount ($)",15)
        key = 'midgroup'
        for name in sorted(sums[key]['counts'].keys()):
            counts = int(sums[key]['counts'][name])
            amount = int(sums[key]['amount'][name])
            print "  ",nice(name, 50),nice(counts,10),nice(amount,15)
        print ''
        
        print nice("---- SuperGroups ----",50),nice("Counts",10),nice("Amount ($)",15)
        key = 'supergroup'
        for name in sorted(sums[key]['counts'].keys()):
            counts = int(sums[key]['counts'][name])
            amount = int(sums[key]['amount'][name])
            print "  ",nice(name, 50),nice(counts,10),nice(amount,15)
        print ''
        
            
    return sums



##########################################
#
# Write CSV
#
##########################################
def writeCSV(data, filename = "Portfolio/records.csv"):
    for k,v in data.iteritems():
        try:
            int(v['year'])
        except:
            raise ValueError("Year is not an integer:",v['year'])
            
        try:
            float(v['amount'])
        except:
            raise ValueError("Amount is not an float:",v['amount'])
            
        try:
            getDate(v['date'])
        except:
            raise ValueError("Date is not valid:",v['date'])


    records    = sortRecords(data)
    out_file   = open(filename, "w")
    fieldnames = records[0].keys()
    writer     = csv.DictWriter(out_file, delimiter=',', fieldnames=fieldnames)
    writer.writeheader()
    for row in records:
        for k in row.keys():
            if row[k] == None:
                row[k]=0
        writer.writerow(row)
    print "Writing",len(data),"entries to",out_file.name
    out_file.close()