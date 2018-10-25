# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 10:39:07 2015

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

import json
import fileio
import argparse
from location import getBaseLocation
from strdate import getDate, getMDY, getYearMonth
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput
from edits import applyEdits, getPayeeCategory, checkPayee, checkCategory

    
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


    # Statement
    output['statement']=getYearMonth(output['date'])


    # Original
    output['original'] = row['Category']


    # Amount
    if row.get('Amount'):
        output['amount']=float(row['Amount'])


    # Type
    if row.get('Category'):
        flag=row['Category']
        if flag == "HouseDownPayment":
            output['type']="Receipt"
        elif flag == "BankCharge":
            output['type']="Interest"
        else:
            print "Type:",flag,"not known."
            raise()


    # Payee
    if row.get("Payee"):
        output['original'] = row['Payee']
        payee=row['Payee']


    checkPayee(payee, row)
    output['payee'] = payee


    # Category
    category = getPayeeCategory(payee)
    output['category'] = category
    

    # Transfer, Category, and Payee
    if output['type'] == "Receipt":
        if output['amount'] > 0:
            output['transfer'] = "INTO"
            output['category'] = account+"ContributionReceipt"
            output['payee']    = "ChaseBankAccount"
        else:       
            output['transfer'] = "OUT"
            output['category'] = account+"ContributionPayment"
            output['payee']    = "ChaseBankAccount"
    else:
        output['transfer'] = "NONE"
        output['payee']    = account

    if output['type'] == "Interest":
        output['category'] = account+"Interest"
        output['payee']    = account
        

    checkCategory(category, row)


    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k," in row!"
            print "row ->",row
            print "out ->",output
            exit()

    return output




##########################################
#
# Read missing json data
#
##########################################
def parseMissing(filename, account):
    keys=None
    lldata = fileio.get(filename)
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
        dataval['type'] == dataval['type'].replace("HSBC", "")

        if dataval['type'] == "Deposit":
            if dataval['amount'] > 0:
                dataval['transfer'] = "INTO"
                dataval['category'] = account+"ContributionReceipt"
                dataval['payee']    = "ChaseBankAccount"
            else:       
                dataval['transfer'] = "OUT"
                dataval['category'] = account+"ContributionPayment"
                dataval['payee']    = "ChaseBankAccount"
        elif dataval['type'] == "Payment":
            if dataval['amount'] > 0:
                dataval['transfer'] = "INTO"
                dataval['category'] = account+"ContributionReceipt"
                dataval['payee']    = "ChaseBankAccount"
            else:       
                dataval['transfer'] = "OUT"
                dataval['category'] = account+"ContributionPayment"
                dataval['payee']    = "ChaseBankAccount"
        elif dataval['type'] == "ChaseDeposit":
            if dataval['amount'] > 0:
                dataval['transfer'] = "INTO"
                dataval['category'] = account+"ContributionReceipt"
                dataval['payee']    = "ChaseBankAccount"
                dataval['type'] == "Receipt"
            else:       
                dataval['transfer'] = "OUT"
                dataval['category'] = account+"ContributionPayment"
                dataval['payee']    = "ChaseBankAccount"
                dataval['type'] == "Payment"
        else:
            dataval['transfer'] = "NONE"
            dataval['payee']    = account
                
                   
        dataval['date']   = getDate(dataval['date'])
        mon,day,year      = getMDY(dataval['date'])
        dataval['month']  = int(mon)
        dataval['day']    = int(day)
        dataval['year']   = int(year)


        data.append(dataval)
        i += 1
        continue


    return data


def createHSBCCSV(account = "HSBC", debug = False):
    basedir = getBaseLocation()
    fileio.removeSubPattern(basedir, [account, "csv"], ".csv", debug)
    files = fileio.findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata = fileio.get(ifile)
        data=[]
        nrows=0
        for row in csvdata:
            nrows += 1
            vals=parseRow(row, account)
            data.append(vals)
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        data = applyEdits(data, debug=debug)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)

    ifile = fileio.findSubExt(basedir, [account, "missing"], ".dat")
    data=parseMissing(ifile[0], account)
    nrows = len(data)
    print "\tProcessed",len(data),"/",nrows,"rows in",ifile
    data = applyEdits(data, debug=debug)
    writeCSVFile(data, basedir=basedir, account=account, debug=debug)



def createHSBCJSON(account = "HSBC", debug = False):
    basedir = getBaseLocation()
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)



def showHSBCRecords(account = "HSBC", debug = False):
    basedir = getBaseLocation()
    showSummary(account, basedir=basedir, debug=debug, showYear = False)


    
def run(debug = False):
    createHSBCCSV(debug=debug)
    createHSBCJSON(debug=debug)


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

    account="HSBC"
    if args.run:
        createHSBCCSV(account=account, debug=args.debug)
        createHSBCJSON(account=account, debug=args.debug)
    if args.csv:
        createHSBCCSV(account=account, debug=args.debug)
    if args.json:
        createHSBCJSON(account=account, debug=args.debug)
    if args.show:
        showHSBCRecords(account=account, debug=args.debug)