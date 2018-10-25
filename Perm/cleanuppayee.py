def cleanUpPayee(payee):

    newpayee=payee
    
    #
    # Return if check
    #
    #if newpayee[:5].title() == "Check":
    #    return False,newpayee.replace(" ","_")

    
    #
    # Remove `POS_____`
    #
    newpayee=newpayee.replace(" ","_")
    

    #
    # Remove `'`
    #
    newpayee=newpayee.replace("'","")
    

    #
    # Remove `CVS/`
    #
    newpayee=newpayee.replace("CSV/","CSV")
    

    #
    # Remove `&`
    #
    newpayee=newpayee.replace("&","And")


    #
    # Remove `____`
    #
    test="____"
    pos=newpayee.find(test)
    if pos > -1:
        newpayee=newpayee[:pos]
    
    
    #
    # Remove `POS_____`
    #
    test="POS_____"
    pos=newpayee.find(test)
    if pos > -1:
        newpayee=newpayee[pos+len(test):]
    
    #
    # Remove `PAYPAL`
    #
    test="PAYPAL"
    pos=newpayee.find(test)
    if pos > -1:
        newpayee=newpayee[pos+len(test)+2:]

    #   
    #Remove '_#->'
    #
    test="_#"
    pos  = newpayee.find(test)
    posC = newpayee.find("Check")
    posP = newpayee.find("_Pin_")
    if pos > -1 and posC == -1 and posP == -1:
        newpayee = newpayee[:pos]


    #
    # Remove cities/states
    #
    cities_states=[]
    cities_states.append("_Bronx_Ny");
    cities_states.append("_New_York_Ny");
    cities_states.append("_New_Yorkd_Ny");
    cities_states.append("_Naperville_Il");
    cities_states.append("_Santa_Barbara_Ca");
    cities_states.append("_Smithtown_Ny");
    cities_states.append("_Uniondale_Ny");
    cities_states.append("_East_Setauket_Ny");
    cities_states.append("_Elkhart_In");
    cities_states.append("_Atlanta_Ga");
    cities_states.append("_Amittyville_Ny");
    cities_states.append("_Centereach_Ny");
    cities_states.append("_Cranbury_Nj");
    cities_states.append("_Aurora_Il");
    cities_states.append("_Philadelphia_Pa");
    cities_states.append("_Cambridge_Ma");
    cities_states.append("_Port_Jefferso_Ny");
    cities_states.append("_Bartonsville_Pa");
    cities_states.append("_Geneva_Il");
    cities_states.append("_Downers_Grove_Il");
    cities_states.append("_Provincetown_Ma");
    cities_states.append("_Huntington_St_Ny");
    cities_states.append("_St._Louis_Mo");
    cities_states.append("_Damariscotta_Me");
    cities_states.append("_Bootbay_Harb_Me");
    cities_states.append("_Kennett_Squar_Pa");
    cities_states.append("_Kihei_Hi");
    cities_states.append("_Chicago_Il");
    cities_states.append("_CHICAGO_IL");
    cities_states.append("_Brooklyn_Ny");
    cities_states.append("_Morristown_Nj");
    cities_states.append("_Mount_Sinai_Ny");
    cities_states.append("_Salt_Lake_Cit_Ut");
    cities_states.append("_Wading_River_Ny");
    cities_states.append("_Province_Town_Ma");
    cities_states.append("_Piscataway_Nj");
    cities_states.append("_Miller_Place_Ny");
    cities_states.append("_Selden_Ny");
    cities_states.append("_Riverhead_Ny");
    cities_states.append("_Setauket_Ny");
    cities_states.append("_Riverhead_Ny");
    cities_states.append("_Hyannis_Ma");
    cities_states.append("_Brighton_Ma");
    cities_states.append("_Williamsburg_Va");
    cities_states.append("_Upton_Ny");
    cities_states.append("_Flushing_Ny");
    cities_states.append("_San_Francisco_Il");
    cities_states.append("_Woodland_Hill_Ca");
    cities_states.append("_Irvington_Ny");
    cities_states.append("_Jamaica_Ny");
    cities_states.append("_Los_Angeles_Ca");
    cities_states.append("_Boothbay_Har_Me");
    cities_states.append("_Yarmout_Ma");
    cities_states.append("_Flushing_Ny");
    cities_states.append("_East_Lansing_Mi");
    cities_states.append("_E_Lansing_Mi");
    cities_states.append("_Newark_Nj");
    cities_states.append("_Woodside_Ny");
    cities_states.append("_Prt_Jefferson_Ny");
    cities_states.append("_Huntington_Ny");
    cities_states.append("_Warrenville_Il");
    cities_states.append("_Pt_Jefferson_Ny");
    cities_states.append("_Mission_Viejo_Ca");
    cities_states.append("_Washington_Dc");
    cities_states.append("_Brookfield_Il");
    cities_states.append("_Desplaines_Il");
    cities_states.append("_Bolingbrook_Il");
    cities_states.append("_New_York_City_Ny");
    cities_states.append("_Prt_Jeff_Sta_Ny");
    cities_states.append("_Ny_Ny");
    cities_states.append("_NEW_YORK_NY");
    cities_states.append("_Stony_Brook_Ny");
    cities_states.append("_Stony_Brook_Ny");
    cities_states.append("_Mount_Prospec_Il");
    cities_states.append("_Winfield_Il");

    for citystate in cities_states:
        if citystate in newpayee:
            pos=newpayee.find(citystate)
            newpayee=newpayee[:pos]

    if newpayee != payee:
        return True,newpayee
    else:
        return False,newpayee
