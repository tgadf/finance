# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 10:39:07 2015

@author: tgadfort
"""

import sys
import argparse
import json
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

from fileio import get
from search import removeSubPattern, findSubExt
from strdate import getDate, getMDY
from cleanuppayee import cleanUpPayee
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput
from edits import applyEdits, getCorrPayee, isKnownPayee, getPayeeCategory, checkCategory, checkPayee

    
##########################################
#
# Parse output from CSV to produce CSV
#
##########################################
def parseRow(row, account):
    output = initCSVoutput()
    output['account'] = account



    # Date
    if row.get('Date'):
        output['date']=getDate(row['Date'])
        mon,day,year     = getMDY(output['date'])
        output['month']  = int(mon)
        output['day']    = int(day)
        output['year']   = int(year)

        
        
    # Description
    if row.get('Statement'):
        output['statement']=row['Statement']



    # Amount
    if row.get('Amount'):
        output['amount']=float(row['Amount'])



    # Type
    if row.get('Type'):
        flag=row['Type']
        if flag == "Pur":
            output['type']="CreditPurchase"
        elif flag == "$":
            output['type']="CreditCashAdvance"
        elif flag == "Pay":
            output['type']="CreditPayment"
        elif flag == "C":
            output['type']="CreditRedemption"
        elif flag == "F":
            output['type']="CreditFee"
        else:
            print "Type:",flag,"not known."
            raise()

    # Payee
    if row.get("Payee"):
        output['original'] = row['Payee']
        retval,cleanpayee=cleanUpPayee(row['Payee'])
        payee=getCorrPayee(cleanpayee)

        if row.get("Known"):
            known=row['Known']
            if known != payee:
                if isKnownPayee(known):
                    payee=known
                    
                    
        checkPayee(payee, row)
        output['payee'] = payee



    # Category
    category = getPayeeCategory(payee)
    output['category'] = category
    checkCategory(category, row, payee)
              


    # Transfer
    output['transfer'] = None
    if output['category'] == "CreditCardReceipt":
        output['transfer'] = "INTO"
    else:
        output['transfer'] = "NONE"
        


    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k," in row!"
            print "row ->",row
            print "out ->",output
            raise()

    return output



##########################################
#
# Read missing json data
#
##########################################
def parseMissing(filename):
    keys=None
    lldata = get(filename)
    print("Read",len(lldata),"lines from",filename)
    i=0
    data=[]
    while i < len(lldata):
        if len(lldata[i]) < 2:
            i += 1
            continue

        if lldata[i][0] == "#":
            i += 1
            continue

        try:
            row=lldata[i]
        except:
            break
        if i == 0:
            vals=row.split('\t')
            keys=json.loads(vals[len(vals)-1])
            i += 1
            continue
        if i == 1:
            i += 1
            continue


        vals=row.split('\t')
        if len(vals) == 4:
            values  = json.loads(vals[len(vals)-1])
        elif len(vals) == 2:
            values  = json.loads(vals[len(vals)-1])

        dataval={}
        for k in range(len(keys)):
            key=keys[k]
            val=values[k]
            dataval[key] = val

        # Null the original payment info
        dataval['original'] = "Missing"

        if dataval['type'] == "MissingCreditPayment":
            if dataval['amount'] > 0:
                dataval['transfer'] = "OUT"
                dataval['category'] = "CreditCardReceipt"
            else:       
                dataval['transfer'] = "INTO"
                dataval['category'] = "CreditCardReceipt"
        else:
            dataval['transfer'] = "NONE"

        dataval['date']   = getDate(dataval['date'])
        mon,day,year      = getMDY(dataval['date'])
        dataval['month']  = int(mon)
        dataval['day']    = int(day)
        dataval['year']   = int(year)
                
        checkCategory(dataval['category'], row=None, payee=dataval['payee'])
        checkPayee(dataval['payee'], row=None)


        data.append(dataval)
        i += 1
        continue


    return data



def createCreditCSV(basedir = '/Users/tgadfort/Documents/finance',
                    account = "Credit", debug = False):
    removeSubPattern(basedir, [account, "csv"], ".csv", debug)
    
    ifile = findSubExt(basedir, [account, "missing"], ".dat")
    data=parseMissing(ifile[0])
    nrows = len(data)
    print "\tProcessed",len(data),"/",nrows,"rows in",ifile
    data = applyEdits(data, debug=debug)
    writeCSVFile(data, basedir=basedir, account=account, debug=debug)
    
    files = findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata = get(ifile)
        data=[]
        nrows=0
        for row in csvdata:
            nrows += 1
            if row['Entry'].count("#") > 0:
                continue
            vals=parseRow(row, account)
            data.append(vals)
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        data = applyEdits(data, debug=debug)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)



def createCreditJSON(basedir = '/Users/tgadfort/Documents/finance',
                    account = "Credit", debug = False):
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)

def showCreditRecords(basedir = '/Users/tgadfort/Documents/finance',
                     account = "Credit", debug = False):
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
def run(debug = False):
    createCreditCSV(debug=debug)
    createCreditJSON(debug=debug)


###############################################################################
#
# Main()
#
###############################################################################
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-run', action="store_true", dest='run',  default=False, help='Produce csv files and create JSON.')
    parser.add_argument('-csv', action="store_true", dest='csv',  default=False, help='Produce csv files from original.')
    parser.add_argument('-json',action="store_true", dest='json', default=False, help='Produce json files from the csv files.')
    parser.add_argument('-show',action="store_true", dest='show', default=False, help='Show records from json file.')
    parser.add_argument('-debug',action="store_true", dest='debug', default=False, help='Debug info.')
    args = parser.parse_args()

    account="Credit"
    if args.run:
        createCreditCSV(account=account, debug=args.debug)
        createCreditJSON(account=account, debug=args.debug)
    if args.csv:
        createCreditCSV(account=account, debug=args.debug)
    if args.json:
        createCreditJSON(account=account, debug=args.debug)
    if args.show:
        showCreditRecords(account=account, debug=args.debug)