import sys
import getopt
import json
import pprint
import argparse
import glob
import os
import csv
import hashlib

from Perm import id

allrecs = {}
fixrecs = {}

def getRecord(recs):
    vals=recs.split(',')
#    print vals
    keys=["num", "data", "amount", "category", "payee", "id", "key", "end"]
    #    rec = zip(keys, vals)
    rec = {}
    rec["num"]  = int(vals[0])
    rec["date"] = str(vals[1])
    rec["amount"] = float(vals[2])
    rec["category"] = str(vals[3])
    rec["payee"] = str(vals[4])
    rec["status"] = int(vals[5])
    rec["id"] = int(vals[6])
    rec["key"] = str(vals[7])
    return rec
#    print rec

def getMiniHash(rec):
    m = hashlib.md5()
    m.update(rec["account"])
    m.update(rec["date"])
    m.update(str(rec["amount"]))
    m.update(str(rec["num"]))
    return m.hexdigest()

def getHash(rec):
    m = hashlib.md5()
    m.update(rec["account"])
    m.update(rec["category"])
    m.update(rec["payee"])
    m.update(rec["date"])
    m.update(str(rec["amount"]))
    m.update(str(rec["num"]))
    return m.hexdigest()


def lookForNewEntries(source, recs):
    newPayees = []
    newCategories = []
    for key in recs:
        rec     = allrecs[key]
        cat     = rec["category"]
        payee   = rec["payee"]
        catid   = id.getCategoryID(cat)
        payeeid = id.getPayeeID(payee)

        if catid == -1:
            if not cat in newCategories:
                #print "Category",cat,"is unknown..."
                newCategories.append(cat)
        if payeeid == -1:
            if not payee in newPayees:
                #print "Payee",payee,"is unknown..."
                newPayees.append(payee)

    if len(newPayees) > 0 or len(newCategories) > 0:
        print ""
        print "Input          --->",source
        print "New Payees     --->",newPayees
        print "New Categories --->",newCategories        
        json.dump(newPayees, open("check/newPayees.json", "w"))
        json.dump(newCategories, open("check/newCategories.json", "w"))
        return True
    else:
        return False

    
def examine(account, myfile):
    with open(myfile, 'rb') as csvfile:
        show=True
        save=False
        reader = csv.reader(csvfile, delimiter=' ', quotechar='|')
        filerecs = {}
        for row in reader:
            if show:
                print len(allrecs),'\t',myfile
                show=False
                save=True

#            print type(row),row
            rec=getRecord(row[0])
            rec["account"] = account
            #rec["minihash"] = getMiniHash(rec)
            key=getMiniHash(rec)
            #            print key
            if allrecs.get(key):
                print "Duplicate!!!!"
                print key
                print rec
                print allrecs[key]
                exit()
            else:
                allrecs[key] = rec
                filerecs[key] = rec

        return filerecs


    
def examineMini(account, myfile):
    with open(myfile, 'rb') as csvfile:
        show=True
        save=False
        reader = csv.reader(csvfile, delimiter=' ', quotechar='|')
        filerecs = {}
        for row in reader:
            if show:
                print len(allrecs),'\t',myfile
                show=False
                save=True

#            print type(row),row
            rec=getRecord(row[0])
            rec["account"] = account
            fullkey=getHash(rec)
            minikey=getMiniHash(rec)
            if allrecs.get(minikey):
                print "Changing",allrecs[minikey],"to",rec
                allrecs[fullkey] = rec
                del allrecs[minikey]

        return filerecs

def replaceUnparsedRecords(change, data, replaceFileName):
    print 'Loaded',len(data),'records.'
    nreplace=0
    nunknown=0
    for key in data:
        rec     = data[key]
        cat     = rec["category"]
        payee   = rec["payee"]
        catid   = id.getCategoryID(cat)
        payeeid = id.getPayeeID(payee)

        if change == "payees":
            if payeeid == -1:
                suggestedfix=id.getCorrPayee(payee)
                if not payee == suggestedfix:
                    rec["payee"] = suggestedfix
                    print "Payee",payee,"is unknown. Replaced --->",suggestedfix
                    nreplace += 1
                else:
                    print "Payee",payee,"is unknown. No suggestion is available."
                    print rec
                    nunknown += 1

        if change == "categories":
            if catid == -1:
                suggestedfix=id.getPayeeCategory(payee)
                if not payee == suggestedfix:
                    rec["category"] = suggestedfix
                    print "Payee",payee,"is unknown. Replaced --->",suggestedfix
                    nreplace += 1
                else:
                    print "Payee",payee,"is unknown. No suggestion is available."
                    print rec
                    nunknown += 1

    
    print 'Replaced',nreplace,'entries.'
    print 'There are',nunknown,'unknown entries.'
    if nreplace > 0:
        json.dump(data, open(replaceFileName, 'w'))
        print 'Wrote',replaceFileName,'with',len(data),'entries.'
    else:
        print 'Did not write because nothing was changed.'

    
def lookForUnparsedRecord(data):
    newPayees = []
    newCategories = []
    for key in data:
        rec     = data[key]
        cat     = rec["category"]
        payee   = rec["payee"]
        catid   = id.getCategoryID(cat)
        payeeid = id.getPayeeID(payee)

        if catid == -1:
            if not cat in newCategories:
                #print "Category",cat,"is unknown..."
                newCategories.append(cat)
        if payeeid == -1:
            if not payee in newPayees:
                newPayees.append(payee)

    if len(newPayees) > 0 or len(newCategories) > 0:
        print ""
        print "New Categories --->",newCategories
        print "New Payees     --->",newPayees
        for payee in newPayees:
            print "Payee",payee,"is unknown. Suggest --->",id.getCorrPayee(payee)
        json.dump(newPayees, open("check/newPayees.json", "w"))
        json.dump(newCategories, open("check/newCategories.json", "w"))
        return True
        exit()
    else:
        return False
        

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-inspect', action="store_true", dest='check', default=False, help='Look for unparsed records')
    parser.add_argument('-n', action="store_true", dest='dek', default=False, help='Loop over new files')
    parser.add_argument('-replace-payees', action="store_true", dest='replace', default=False, help='Replace payees in records.')    
    parser.add_argument('-replace-categories', action="store_true", dest='replace_categories', default=False, help='Replace payees in records.')
    parser.add_argument('-find-payee', nargs=1)
    parser.add_argument('-show-categories', action="store_true", dest='showcats', default=False, help='Show categories.')
    parser.add_argument('-find-category', nargs=1)
    parser.add_argument('-rm-payee', nargs=1)
    parser.add_argument('-add-payee', nargs=1)
    parser.add_argument('-add-payee-category', nargs=2)
    parser.add_argument('-add-corr-payee', nargs=2, help='<Original Name> <What I Want>')
    parser.add_argument('-show-corr-payee', nargs=1, help='<What I Want>')
    parser.add_argument('-rm-corr-payee', nargs=1)
    parser.add_argument('-add-payee-corr', nargs=2, help='Adds an expression and maps it a payee')
    args = parser.parse_args()
    print args

    
    #
    # If we want to find a payee
    #
    if not args.find_payee == None:
        result=id.getPayeeID(args.find_payee[0], True)
        if result == -1:
            print 'No entry for',args.find_payee[0]
        else:
            print args.find_payee[0],'is entry #'+str(result),'('+id.getPayee(result)+')'
        exit()


    #
    # If we want to show categories
    #
    if args.showcats:
        id.showCategories()


    #
    # If we want to find a category
    #
    if not args.find_category == None:
        result=id.getCategoryID(args.find_category[0], True)
        if result == -1:
            print 'No entry for',args.find_category[0]
        else:
            print args.find_category[0],'is entry #'+str(result),'('+id.getCategory(result)+')'

    
    #
    # If we want to add new entries
    #
    if not args.add_payee == None:
        if id.addPayee(args.add_payee[0]) == False:
            print 'Entry for '+args.add_payee[0]+' could not be added.'

    
    #
    # If we want to add new entries
    #
    if not args.add_payee_category == None:
        payee=args.add_payee_category[0]
        cat=args.add_payee_category[1]
        if id.addPayeeCategory(payee, cat) == False:
            print 'Entry for',payee,'->',cat,'could not be added.'
        else:
            print 'Added',cat,'as category for',payee
        exit()
    
    #
    # If we want to add correction new entries
    #
    if not args.add_corr_payee == None:
        payee=args.add_corr_payee[0]
        corr=args.add_corr_payee[1]
        if id.addCorrPayee(payee, corr) == False:
            print 'Could not map('+payee+') to '+corr
            print 'First add',corr,'to list of payees'
            if id.addPayee(corr) == False:
                print 'Entry for '+corr+' could not be added.'
            else:
                print 'Added',corr,'to list of payees.'
                print 'Try again now.'
        else:
            exit()
    
    #
    # If we want to add correction new entries
    #
    if not args.show_corr_payee == None:
        payee=args.add_corr_payee[0]
        if id.showCorrPayee(payee) == False:
            exit()
            print 'Could not map('+payee+') to '+corr
            print 'First add',corr,'to list of payees'
            if id.addPayee(corr) == False:
                print 'Entry for '+corr+' could not be added.'
            else:
                print 'Added',corr,'to list of payees.'
                print 'Try again now.'
        else:
            exit()

    
    #
    # If we want to remove correction for new entries
    #
    if not args.rm_corr_payee == None:
        corr=args.rm_corr_payee[0]
        if id.rmCorrPayee(corr) == False:
            print 'Could not remove(',corr,')'
        else:
            print 'Removed',corr,'.'
        exit()

    
    #
    # If we want to remove new entries
    #
    if not args.rm_payee == None:
        if id.rmPayee(args.rm_payee[0]) == False:
            print 'Entry for '+args.rm_payee[0]+' could not be removed.'
    
    #
    # If we want to check new entries
    #
    if args.replace or args.replace_categories:
        fdir=os.path.dirname(os.path.realpath(__file__))
        jsonfile=fdir+"/fixrecords.json"
        fixedjsonfile=fdir+"/fixrecords.tmp.json"
        infile=json.load(open(jsonfile, 'r'))
        if args.replace:
            replaceUnparsedRecords("payees", infile, fixedjsonfile)
        if args.replace_categories:
            replaceUnparsedRecords("categories", infile, fixedjsonfile)
    
    
    #
    # If we want to check new entries
    #
    if args.check:
        fdir=os.path.dirname(os.path.realpath(__file__))
        jsonfile=fdir+"/fixrecords.json"
        with open(jsonfile, 'r') as infile:
            lookForUnparsedRecord(json.load(infile))

    if args.dek:
        accounts=["Chase", "Credit", "HSBC"]
        for account in accounts:
            for myfile in glob.glob(account+"/*.csv"):
                recs=examine(account, myfile)
                if len(recs) > 0:
                    newfile=myfile.replace(".csv", ".json")
                    json.dump(recs, open(newfile, "w"))
                    if lookForNewEntries(myfile, recs):
                        print "Errors in records."

        json.dump(allrecs, open("records.json", "w"))

        for account in accounts:
            for myfile in glob.glob(account+"/*.csv.formatcategory"):
                recs=examineMini(account, myfile)
                if len(recs) > 0:
                    newfile=myfile.replace(".csv", ".json")
                    json.dump(recs, open(newfile, "w"))
                    if lookForNewEntries(myfile, recs):
                        print "Errors in records."
                        exit()

        json.dump(allrecs, open("fixrecords.json", "w"))


