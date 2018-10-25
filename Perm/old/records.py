# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 15:59:54 2015

@author: tgadfort
"""

import sys
import yaml
import json
import argparse
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Python')
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')

from cleanuppayee import cleanUpPayee
from grouping import getGroupMapping


def f(): raise Exception("Found exit()")

def makeDate(date):
    try:
        tformat = "%m/%d/%Y"
        dayval   = datetime.datetime.strptime(date, tformat)
        return dayval
    except:
        return None
        


def testFile(yfile):
    try:
        print "Checking",yfile
        yaml.load(open(yfile))
        print yfile,"looks good."
    except:
        print "Could not parse",yfile
        
        

def addCat(yfile):        
    print "Loading",yfile
    ydata=yaml.load(open(yfile))
    try:
        ddata=ydata["X"]
    except:
        ddata=ydata
        
    catdatafile = "Perm/schema/categoryCodeForPayees.json"
    catdata = json.load(open(catdatafile))
    categories={}
    
    
    alldata={}
#    alldata["X"]={}
    for payee,corrvals in ddata.iteritems():
        pdata={}
        pdata["corrections"] = corrvals
        if catdata.get(payee):
            pdata["category"]    = str(catdata[payee])
            categories[catdata[payee]] = 1
        else:
            pdata["category"]    = None
#            print "Missing"
#        alldata["X"][payee] = pdata
        alldata[payee] = pdata
#        print pdata
        
    yfile = yfile.replace("CorrData", "CorrPayeeData")
#    yfile += ".tmp"
    yaml.dump(alldata, open(yfile, "w"), encoding=None, default_flow_style=False, allow_unicode = True)        
    print "Re-Wrote",yfile

        
        
def newFile(yfile):
    try:
        print "Loading",yfile
        ydata=yaml.load(open(yfile))
        print "Writing",yfile
        yaml.dump(ydata, open(yfile, "w"), encoding=None, default_flow_style=False, allow_unicode = True)        
        print "Re-Wrote",yfile
    except:
        print "Could not rewrite",yfile



def oldFile(yfile):
    ddata = yaml.load(open(yfile))
    try:
        pdata = ddata["X"]
    except:
        pdata = ddata
    corrdata={}
    payeedata={}
    catdata={}
    reversemap={}
    
    
    categoryMap = getGroupMapping()

    for k,v in pdata.iteritems():
        payee    = k
        try:
            corrvals = v['corrections']
        except:
            print "Could not find corrections for",k
            f()
        try:
            category = v['category']
        except:
            print "Could not find category for",k
            f()

        if categoryMap.get(category) == None:
            raise ValueError("Category:",category,"for payee:",k,"not in approved list.")
        if corrvals:
            for corrval in corrvals:
                corrdata[corrval] = payee
        payeedata[payee] = 1
        catdata[payee]   = category
        
    
        
    catmapdatafile  = "Perm/indivcatmap.json"
    print "Wrote",len(reversemap),"to",catmapdatafile
    json.dump(reversemap, open(catmapdatafile, "w"))    
            
    corrdatafile  = "Perm/correctedPayeeNames.json"
    print "Wrote",len(corrdata),"to",corrdatafile
    json.dump(corrdata, open(corrdatafile, "w"))    
    
    payeedatafile = "Perm/payee.json"
    print "Wrote",len(payeedata),"to",payeedatafile
    json.dump(payeedata, open(payeedatafile, "w"))    

    catdatafile = "Perm/categoryCodeForPayees.json"
    print "Wrote",len(catdata),"to",catdatafile
    json.dump(catdata, open(catdatafile, "w"))    
    


def resetFile(yfile):
    payeedata = json.load(open("Perm/payee.json"))
    corrdata  = json.load(open("Perm/correctedPayeeNames.json"))
    payees = sorted(payeedata.keys())
    
    payees = [str(x) for x in payees]
    pdata={}
    for k,v in corrdata.iteritems():
        payee = str(v)
        corr  = str(k)
        if pdata.get(payee) == None:
            pdata[payee] = []
            print "No",payee
        
        pdata[payee].append(corr)
    #    pdata[payee].append([corr])
    
    ddata = {}
    ddata['X'] = pdata
    
    print "Dumping data to",yfile
    yaml.dump(ddata, open(yfile, "w"), encoding=None, default_flow_style=False, allow_unicode = True)



def newPayees(pfile):
    newvals={}
    pdata=json.load(open(pfile))
    for pval in pdata:
        newvals[pval[0]] = pval
    keys = sorted(newvals.keys())
    for newval in keys:
        val = newvals[newval]
        keyval = newval
        while len(keyval) < 50:
            keyval += " "
        print keyval[:50],'  ',val[1][1][0],'\t',val[1][0]


def checkMissing(missing, yfile):
    fdata=open(missing).readlines()
    fdata=[x.replace("\n", "") for x in fdata]
    i=0
    pvals={}
    while i < len(fdata):
        try: line = fdata[i]
        except: break
        while line[0] != "#":
            i += 1
            try: line = fdata[i]
            except: break
            continue
        
        if line[0] != "#": continue
        echo=line
        i += 1
        line = fdata[i]
        payee = None
        if line[1] == "-":            
            oldpayee = line.split(" - ")[1]
 #           print oldpayee
        i += 1
        line = fdata[i]
        if line.find(".csv") != -1 and line.find(":") != -1:
            try:
                vals=line.split(":")[1].split(",")
            except:
                print echo
                print oldpayee
                print line
                f()
            payee = vals[5]
            money = vals[3]
        else:
            continue

        if pvals.get(payee):
            pvals[payee].append([oldpayee, money])
        else:
            pvals[payee] = []
            pvals[payee].append([oldpayee, money])
        print payee,"  -->  ",oldpayee

        i += 1 


    ydata=yaml.load(open(yfile))
    for k,v in pvals.iteritems():
        if ydata.get(str(k)):
            for corrval in v:
#                print k,' ---> ',corrval[0]
                ydata[str(k)]['corrections'].append(str(corrval[0]))
        else:
                print k,' --->  ',corrval


    yaml.dump(ydata, open(yfile, "w"), encoding=None, default_flow_style=False, allow_unicode = True)        
    print "Re-Wrote",yfile
    oldFile(yfile)

    

def showMissing(missing):
    mdata = json.load(open(missing))
    for k,v in mdata.iteritems():
        for it in v:
            state = it[1][1][3]
            dates = it[1][0].split("/")
#            dates = [int(x) for x in dates]            
            dates = [str(x) for x in dates]            
            date  = "20" + dates[2]
            money = str(it[1][1][0])
            money = money.replace("-","\-")
            payee = it[1][1][1]
#            print it
            retval,newpayee=cleanUpPayee(payee)
            print "echo \"#"+date+","+money+" *.csv\""
            print "echo \" - "+newpayee+"\""
            print "grep \""+date+","+money+"\" *.csv"
            print ""
            
            
def findMissing(recordsfile, missing, yfile):
    mdata = json.load(open(missing))
    records = json.load(open(recordsfile))

    missing={}    
    for k,v in mdata.iteritems():
        for it in v:
            state = it[1][1][3]
            dates = it[1][0].split("/")
#            dates = [int(x) for x in dates]            
            dates = [str(x) for x in dates]            
            dates[2]  = "20" + dates[2]
            date  = "/".join(dates)
            money = it[1][1][0]
            payee = it[1][1][1]

        
            tdate = makeDate(date)
            for k,v in records.iteritems():
                if v['statement'] == state:
                    ndate = makeDate(v['date'])
                    if ndate == tdate:
                        if abs(money - v['amount']) < 1:
                            if missing.get(v['payee']) == None:
                                missing[v['payee']] = []
                            missing[v['payee']].append(it[0])
#                            print v['payee'],'  --->  ',it[0]
                            

    ydata=yaml.load(open(yfile))
    for k,v in missing.iteritems():
        if k == "UknownVacationCheck":
            k = "UnknownVacationCheck"
        if ydata.get(str(k)):
            for corrval in v:
                print k,' ---> ',corrval
                if ydata[str(k)]['corrections'] == None:
                    ydata[str(k)]['corrections'] = []
                ydata[str(k)]['corrections'].append(str(corrval))
        else:
                print k,' ->  ',v
                f()


    f()
    yaml.dump(ydata, open(yfile, "w"), encoding=None, default_flow_style=False, allow_unicode = True)        
    print "Re-Wrote",yfile
    oldFile(yfile)
                            
                            
#        print k,v
#    f()

##################################################################
##
## main
##
##################################################################
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    #parser.add_argument('-get-games', dest='getgames', nargs=1, help='Base directory name.')
    parser.add_argument('-compile', action="store_true", dest='compile', help='Get SFrefence data.')
    parser.add_argument('-cat', action="store_true", dest='cat', help='Get SFrefence data.')
    parser.add_argument('-reset', action="store_true", dest='reset', help='Get SFrefence data.')
    parser.add_argument('-test', action="store_true", dest='test', help='Get SFrefence data.')
    parser.add_argument('-old', action="store_true", dest='old', help='Get SFrefence data.')
    parser.add_argument('-file', nargs=1, dest='file', help='Get SFrefence data.')
    parser.add_argument('-find', action="store_true", dest='find', help='Get SFrefence data.')
    args = parser.parse_args()
    print args    
    
    yfile = "Perm/schema/CorrPayeeData.yaml"

#    print "HI"
#    checkMissing("Chase/csv/check.dat", yfile)
#    showMissing("Chase/data/MissingEntries.json")
#    f()

    if args.find:
        findMissing("Portfolio/records.json", "Chase/data/MissingEntries.json", yfile)

    if args.old:
        oldFile(yfile)
        
    if args.file:
        newPayees(args.file[0])
    
    if args.test:
        testFile(yfile)
        
    if args.reset:
        resetFile(yfile)
        
    if args.cat:
        addCat(yfile)

    if args.compile:
        newFile(yfile)

