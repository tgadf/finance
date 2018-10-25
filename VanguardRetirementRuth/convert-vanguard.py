# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 10:39:07 2015

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

import fileio
import argparse
from getter import sortRecords
from strdate import getDate, getYearMonth, getMDY
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput, writeCSVFileFromDict
from edits import applyEdits

    
##########################################
#
# Parse output from CSV to produce CSV
#
##########################################
def stripKeys(row):
    keys=row.keys()
    for k in keys:
        if k != k.strip():
            row[k.strip()] = row[k]
            del row[k]    
    return row

    
def parseRow(row, account):
    output=initCSVoutput()
    output['account'] = account

    row = stripKeys(row)


    # Date
    if row.get('date'):
        output['date']=getDate(row['date'])
        mon,day,year     = getMDY(output['date'])
        output['month']  = int(mon)
        output['day']    = int(day)
        output['year']   = int(year)
        

    # Type
    if row.get('type'):
        output['type']=row['type']


    # Amount
    if row.get('amount'):
        output['amount']=float(row['amount'])


    # Statement
    output['statement']=getYearMonth(output['date'])


    # Original
    output['original']=row['original']


    # Category
    if output['type'] == "Opening Balance":
        output['category'] = account+"OpeningBalance"
        output['payee'] = account
    elif output['type'] == "Interest":
        output['category'] = account+"Interest"
        output['payee'] = account
    elif output['type'] == "Contribution":
        output['category'] = account+"ContributionReceipt"
        output['payee'] = "ChaseBankAccount"
    else:
        raise ValueError("Did not recognize type:",output['type'])


    # Type
    output['type'] = output['category'].replace(account,"")
    output['type'] = output['type'].replace("ContributionReceipt","Contribution")

    # Transfer
    if output['category'] == account+"ContributionReceipt":
        output['transfer']="INTO"
    else:
        output['transfer']="NONE"

          

    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k,"in row!"
            print "  row  ->",row
            print "  out  ->",output
            print "  fund ->",output['fund']
            raise()

    return output


def createVanguardRetirementRuthBalance(basedir = '/Users/tgadfort/Documents/finance',
                                  account = "VanguardRetirementRuth", debug = False):
    fileio.removeSubPattern(basedir, [account, "original"], ".csv", debug)
    files = fileio.findSubExt(basedir, [account, "balance"], ".csv")
    for ifile in files:
        csvdata  = fileio.getCSV(ifile, delimiter="\t", strip=True)
        data=[]
        line={"date": '1/1/2009', "amount": 0.0, "type": "Opening Balance", "original": "custom"}
        data.append(line)
        nrows = 1
        pvalue  = None
        csvdata = sortRecords(csvdata)
        for row in csvdata:
            row = stripKeys(row)

            value = row['Balance'].replace("$","")
            balance = float(value.replace(",",""))
            #print ""
            #print "--->",value
            if pvalue:
                #print "\t--->",balance,'\t',pvalue
                value = balance - pvalue
            else:
                #print "\t--->",balance,'\tNone'
                value = balance
            pvalue = balance
            descr = "Interest"

            ttype = row['Transaction type']
            if ttype == "Conversion To":
                descr = "Opening Balance"
            elif ttype == "Dividend Received":
                descr = ttype
            elif ttype == "Buy":
                descr = ttype
                amt = row['Amount']
                amt = amt.replace("$","")
                amt = float(amt.replace(",",""))
                line={"date": row['Date'], "amount": amt, "type": descr, "original": ttype}
                data.append(line)
                descr = "Interest"
                ttype = "Interest"
                value -= amt
            elif ttype == "Long-term capital gain":
                descr = ttype
            elif ttype == "Short-term capital gain":
                descr = ttype
            else:
                raise ValueError("Unknown type:",ttype)

            line={"date": row['Date'], "amount": value, "type": descr, "original": ttype}
            data.append(line)
            nrows += 1
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        writeCSVFileFromDict(data, account=account, subdir="original", basedir=basedir, debug=debug)



def createVanguardRetirementRuthCSV(basedir = '/Users/tgadfort/Documents/finance',
                    account = "VanguardRetirementRuth", debug = False):
    fileio.removeSubPattern(basedir, [account, "csv"], ".csv", debug)
    files = fileio.findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata  = fileio.getCSV(ifile, delimiter=",")
        data=[]
        nrows=0
        for row in csvdata:
            nrows += 1
            vals=parseRow(row, account)
            if vals:
                data.append(vals)
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        #data = applyEdits(data, debug=debug)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)


def createVanguardRetirementRuthJSON(basedir = '/Users/tgadfort/Documents/finance',
                    account = "VanguardRetirementRuth", debug = False):
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)

def showVanguardRetirementRuthRecords(basedir = '/Users/tgadfort/Documents/finance',
                     account = "VanguardRetirementRuth", debug = False):
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
def run(debug = False):
    createVanguardRetirementRuthCSV(debug=debug)
    createVanguardRetirementRuthJSON(debug=debug)


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

    account="VanguardRetirementRuth"
    if args.run:
        createVanguardRetirementRuthCSV(account=account, debug=args.debug)
        createVanguardRetirementRuthJSON(account=account, debug=args.debug)
    if args.csv:
        createVanguardRetirementRuthCSV(account=account, debug=args.debug)
    if args.json:
        createVanguardRetirementRuthJSON(account=account, debug=args.debug)
    if args.show:
        showVanguardRetirementRuthRecords(account=account, debug=args.debug)