# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 10:39:07 2015

@author: tgadfort
"""

import sys
import argparse
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Python')
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')

from fileio import get
from fsio import setSubFile, setSubDir, isDir
from uid import getHash
from strops import nice
from strdate import countDays, getDate, getMDY, getNearbyDates
from getter import getEdits, getData, saveData
from convertBase import showSummary, writeCSVFileFromDict
from edits import applyEdits, applyEdit, applyManualEdit, createCorrectedPayeeMapping, applySpecificKnownEdits
from grouping import applyGrouping
from search import findPattern, findDirs
from recsearch import findRecords
from show import printRecords



def getCombinedFiles(basedir = "/Users/tgadfort/Documents/finance"):
    accounts = findDirs(basedir)
    files    = []
    for account in accounts:
        accountDir = setSubDir(basedir, [account, "json"], forceExist=False)
        if not isDir(accountDir):
            continue
        files += findPattern(accountDir, "records.json")
    #files = findSubPattern(basedir, ["*", "json"], "records.json")
    try:
        fpath = setSubFile(basedir, "Portfolio", "records.json")
        print "Removing",fpath
        files.remove(fpath)
    except:
        files=files

    try:
        fpath = setSubFile(basedir, ["Missing", "json"], "records.json")
        print "Removing",fpath
        files.remove(fpath)
    except:
        files=files

    try:
        fpath = setSubFile(basedir, ["Loan", "json"], "records.json")
        print "Removing",fpath
        files.remove(fpath)
    except:
        files=files

    return files






###############################################################################
#
# Combine and edit all records.json files into common file
#
###############################################################################
def run(basedir = "/Users/tgadfort/Documents/finance", account = "Portfolio"):
    combineRecords(basedir, account)
    matchTransfers(basedir, account)
    cleanData(basedir, account)


def updateRecords(basedir = "/Users/tgadfort/Documents/finance", account = "Portfolio"):
    data  = getData()
    data  = applyEdits(data, debug = False)
    data  = applyGrouping(records = data)
    saveData(data)
    writeCSVFileFromDict(data, account = account, basename = "records")
    showSummary(account = account, source = data, showYear = False)

    
def combineRecords(basedir = "/Users/tgadfort/Documents/finance", account = "Portfolio"):

    data={}
    nfixed=0
    fixed = getEdits()

    files = getCombinedFiles()

    for ifile in files:
        print "  -->",ifile
        localaccount=ifile[:ifile.find('/')]
        datafile = get(ifile)
        for k,v in datafile.iteritems():
            hashvals,record=combineJSON(k,v,localaccount)
            if not isinstance(hashvals, list):
                hashval = hashvals
                if data.get(hashval):
                    raise ValueError("There is an error with the hash system!")
                data[hashval] = record
            else:
                for j in range(len(hashvals)):
                    if data.get(hashvals[j]) == None:
                        record['key'] = j
                        hashval = hashvals[j]
                        data[hashval] = record
                        break

            if len(data) % 1000 == 0:
                print "\tProcessed",len(data),"entries."


            record = data[hashval]
            if fixed.get(hashval):
                record,isfixed = applyEdit(record, fixed[hashval], debug = False)
                if isfixed: nfixed += 1
            data[hashval] = record


    print "Found",len(data),"entries."
    print "Fixed",nfixed,"entries."

    data = applyGrouping(records = data)
    saveData(data)
    writeCSVFileFromDict(data, account = account, basename = "records")
    showSummary(account = account, source = data, showYear = False)



###############################################################################
#
# Put individual account data into common format for future use
#
###############################################################################
def combineJSON(oldkey, oldoutput, account):    
    output={}
    jvars=["account", "date", "payee", "category", "amount", "key", "type", "entry"]
    for k in jvars:
        if oldoutput.get(k) != None:
            output[k] = oldoutput[k]
        elif oldoutput.get(k.title()) != None:
            output[k] = oldoutput[k.title()]
        else:
            print "Could not find:",k,"in",oldoutput
            print "Keys:",oldoutput.keys()
            raise()

    output['transfer'] = oldoutput.get('transfer')
    output['date']     = getDate(output['date'])
    output['match']    = 0

    if True:
        if oldoutput.get("original"):
            output['original'] = oldoutput.get("original")
        else:
            output['original'] = None

        if oldoutput.get("statement"):
            output['statement'] = oldoutput.get("statement")
        else:
            output['statement'] = None

    output['corrtransfer']=None
    output['saving']  =None
    output['ignore']  =None
    output['comment'] =None
    output['reconciled']=None
    if output['category'].find("(") != -1 or output['category'].find(")") != -1 or output['category'] == "NOCATEGORY":
        print 'old ->',oldoutput
        print 'new ->',output
        raise()

    mon,day,year = getMDY(output['date'])
    output['month']=int(mon)
    output['day']=int(day)
    output['year']=int(year)
    output['amount']=float(output['amount'])

    keys = getHash(output)

    if output.get('key') == None:
        print "NO KEY!"
        print output
        print keys

    return keys,output




###############################################################################
#
# Print Specific Records
#
###############################################################################
def showRecords(records, showHash = False, skipAmount = False, skipComment = False, skipOriginal = False):
    printRecords(records, sort=True, showHash=showHash, skipAmount=skipAmount, 
                 skipComment=skipComment, skipOriginal=skipOriginal)
    
    
###############################################################################
#
# Create Mapping
#
###############################################################################
def createMapping():
    createCorrectedPayeeMapping()



###############################################################################
#
# Print Specific Records
#
###############################################################################
def manualEdit(hashval, key, value):
    applyManualEdit(hashval, key, value)

def setEdits():
    applySpecificKnownEdits()



###############################################################################
#
# Match all incoming/outgoing and conversions for records after processing
#
###############################################################################
def matchTransfers(basedir = "/Users/tgadfort/Documents/finance", account = "Portfolio"):
    tfdb={}
    tfdb["INTO"] = "OUT"
    tfdb["OUT"]  = "INTO"
    tfdb["CNVO"] = "CNVI"
    tfdb["CNVI"] = "CNVO"
    
    tfactdb={}
    tfactdb["Claire529IL"]   = ["Chase", "ChaseFreedom"]
    tfactdb["Claire529NY"]   = ["Chase", "ChaseFreedom"]
    tfactdb["Charlie529IL"]  = ["Chase", "ChaseFreedom"]
    tfactdb["HSBC"]          = ["Chase", "ChaseFreedom"]
    tfactdb["RuthBankAccount"] = ["Chase"]
    tfactdb["Credit"]        = ["Chase", "ChaseFreedom"]
    tfactdb["CreditFreedom"] = ["Chase", "ChaseFreedom"]
    tfactdb["ChaseFreedom"]  = ["Chase"]    
    tfactdb["VanguardRetirementRuth"]   = ["Chase", "ChaseFreedom", "VanguardRetirementRuth"]
    tfactdb["VanguardRetirementThomas"] = ["Chase", "ChaseFreedom", "VanguardRetirementThomas"]
    tfactdb["VanguardRothIRARuth"]      = ["Chase", "ChaseFreedom", "VanguardRothIRARuth"]
    tfactdb["VanguardRothIRAThomas"]    = ["Chase", "ChaseFreedom", "VanguardRothIRAThomas"]
    tfactdb["VanguardTradIRARuth"]      = ["Chase", "ChaseFreedom", "VanguardTradIRARuth"]
    tfactdb["VanguardTradIRAThomas"]    = ["Chase", "ChaseFreedom", "VanguardTradIRAThomas"]
    tfactdb["TIAAFNALThomas"]           = ["FidelityFNAL", "TIAAFNALThomas"]
    tfactdb["FidelityFNAL"]             = "TIAAFNALThomas"
    tfactdb["Chase"]                    = tfactdb.keys()

    tfcatdb={}    
    bases  = ["HSBCContribution", "CreditCard", "CreditCardFreedom"]
    bases += ["VanguardRetirementThomasContribution"]
    bases += ["VanguardRetirementRuthContribution"]
    bases += ["VanguardRothIRAThomasContribution"]
    bases += ["VanguardRothIRARuthContribution"]
    bases += ["VanguardTradIRAThomasContribution"]
    bases += ["VanguardTradIRARuthContribution"]
    bases += ["ChaseFreedom", "RuthBankAccount"]
    for base in bases:
        tfcatdb[base+"Receipt"] = base+"Payment"
        tfcatdb[base+"Payment"] = base+"Receipt"
    bases = ["Charlie529IL", "Claire529IL", "Claire529NY"]
    for base in bases:
        tfcatdb[base+"Receipt"]     = base+"Payment"
        tfcatdb[base+"GiftReceipt"] = base+"Payment"
        tfcatdb[base+"Payment"]     = [base+"Receipt",base+"GiftReceipt"]        
    tfcatdb["TIAAFNALThomasTransfer"] = "FidelityFNALTransfer"
    tfcatdb["FidelityFNALTransfer"] = "TIAAFNALThomasTransfer"
    tfcatdb["TIAAFNALThomasConversion"] = "TIAAFNALThomasConversion"
    print "Getting all transfers..."
    tfs = findRecords(cutTransfer=tfdb.keys())
    print "Getting all transfers... Done"
    print "Found",len(tfs),"transfers."


    tfmap = {}
    i = 0
    recs = {}
    for hashval,v in tfs.iteritems():
        i += 1
        if tfmap.get(hashval): continue
        for drange in [1, 3, 5, 10]:
            #print drange,getNearbyDates(v['date'], drange)
            #print v['date'],v['transfer'],v['account'],v['amount']
            #if v['account'] == "Chase": continue
            recs0 = findRecords(source = tfs, cutDate=getNearbyDates(v['date'], drange))
            recs1 = findRecords(source = recs0, cutTransfer=tfdb[v['transfer']])
            recs2 = findRecords(source = recs1, cutAccount=tfactdb[v['account']])
            if v['account'] == "Credit":
                amt = v['amount']
                recs3 = findRecords(source = recs2, cutAmount=[amt-1,amt+1])
            elif v['account'] == "Chase":
                amt = v['amount']
                recs3a = findRecords(source = recs2, cutAmount=[amt-1,amt+1])
                amt = -1*v['amount']
                recs3b = findRecords(source = recs2, cutAmount=[amt-1,amt+1])
                if recs3b and recs3a:
                    recs3 = recs3a.update(recs3b)
                elif recs3b:
                    recs3 = recs3b
                elif recs3a:
                    recs3 = recs3a
                else:
                    recs3 = {}
            else:
                amt = -1*v['amount']
                recs3 = findRecords(source = recs2, cutAmount=[amt-1,amt+1])
    
            recs4 = findRecords(source = recs3, cutCategory=tfcatdb[v['category']])
    
            if recs0 == None: recs0 = {}
            if recs1 == None: recs1 = {}
            if recs2 == None: recs2 = {}
            if recs3 == None: recs3 = {}
            if recs4 == None: recs4 = {}
            recs[hashval] = [len(recs0),len(recs1),len(recs2),len(recs3),len(recs4)]
            
            if len(recs4) > 0:
                minDays = 11
                minHash = None
                for matchHash, matchRecord in recs4.iteritems():
                    days = countDays(v['date'], matchRecord['date'])
                    if days < minDays:
                        matchHash = getHash(matchRecord)
                        minHash = matchHash
                        minDays = days
    
                if minHash:
                    tfmap[hashval] = minHash
                    tfmap[minHash] = hashval
                    break

        if i % 50 == 0:
            print nice(i,5),nice(v['date'],10),nice(v['transfer'],8),nice(v['account'],30),nice(v['amount'],10),'\t',len(recs0),'\t',len(recs1),'\t',len(recs2),'\t',len(recs3),'\t',len(recs4)

        #if i > 2: break

        
    i = 0
    skips = {}
    for hashval,v in tfs.iteritems():
        if skips.get(hashval):
            continue
        matchHash = tfmap.get(hashval)
        if matchHash:
            #print hashval,  '\t',v['date'],'\t',v['transfer'],'\t',nice(v['account'],18),v['amount'],'\t',tfmap.get(hashval)
            v = tfs[matchHash]
            #print matchHash,'\t',v['date'],'\t',v['transfer'],'\t',nice(v['account'],18),v['amount'],'\t',tfmap.get(matchHash)
            skips[hashval] = 1
            skips[matchHash] = 1
            #print ""
        else:            
            print hashval,  '\t',v['date'],'\t',v['transfer'],'\t',nice(v['account'],30),v['amount'],'\t',recs.get(hashval)
            print v
            print ""
            print ""
            i += 1
            

        if i > 10000: break

    if i > 0:
        print "\nThere were",i,"unmatched transfers. Need to fix this.\n"
    else:
        print "\nThere were no unmatched transfers. All good!\n"

    print "Setting transfer matches for",len(tfs),"records."
    data = getData()
    for hashval in data.keys():
        data[hashval]['match'] = tfmap.get(hashval)

    saveData(data)
    writeCSVFileFromDict(data, account = account, basename = "records")
    print "Done..."
    
    
    
###############################################################################
#
# Clean data for final writing
#
###############################################################################
def cleanData(basedir = "/Users/tgadfort/Documents/finance", 
              account = "Portfolio", debug = False, stop = False):
    print ""
    print "=====> Cleaning data for use by R <====="
    data = getData()
    
    rms = ['corrtransfer', 'ignore', 'entry', 'saving', 'comment', 
           'original', 'reconciled', 'groupval', 'type']
    for hashval in data.keys():
        data[hashval]['id'] = hashval
        for rm in rms:
            try:
                del data[hashval][rm]
            except:
                continue

    writeCSVFileFromDict(data, account = account, basename = "records")



###############################################################################
#
# Main
#
###############################################################################
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-run', action="store_true", dest='run',  default=False, help='Run everything.')
    parser.add_argument('-combine', action="store_true", dest='combine',  default=False, help='Combine records.')
    parser.add_argument('-fixSaved',action="store_true", dest='fixsaved', default=False, help='Fixed saved edits.')
    parser.add_argument('-matchtransfsers',action="store_true", dest='matchtransfers', default=False, help='Match transfers.')
    parser.add_argument('-debug',action="store_true", dest='debug', default=False, help='Debug info.')
    args = parser.parse_args()

    account="Portfolio"
    basedir = "/Users/tgadfort/Documents/finance"


    ######################################
    # Combine and edit all records.json
    ######################################
    if args.combine:
        combineRecords(basedir = basedir, account = account)


    ######################################
    # Match Transfers
    ######################################
    if args.matchtransfers:
        matchTransfers(basedir = basedir, account = account)


    ######################################
    # Fix Saved Edits
    ######################################
    if args.fixsaved:
        fixSavedEdits(basedir = basedir, account = account)