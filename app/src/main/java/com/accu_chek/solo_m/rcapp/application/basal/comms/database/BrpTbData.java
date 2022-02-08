/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BrpTbData
 * Brief: Storage of Basal Rate Profile Time Block
 *
 * Create Date: 11/06/2015
 * $Revision: 24156 $
 * $Author: JacksonHuang $
 * $Id: BrpTbData.java 24156 2015-11-16 05:50:26Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms.database;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;

public class BrpTbData
{
    // Profile ID of affiliated basal rate profile
    private SafetyChannel<Integer> mProfileKeyID = null;
    // End time of this time block
    private SafetyChannel<Integer> mEndTime = null;
    // Insulin unit of this time block
    private SafetyChannel<Integer> mBasal = null;
    
    /**
     * Class Constructor
     * 
     */    
    public  BrpTbData()
    {
        
    }
    
    /**
     * Return attribute: mProfileKeyID
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Returned value of attribute : mProfileKeyID  
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */    
    public SafetyChannel<Integer> getProfileKeyID()
    {
        return mProfileKeyID;
    }
    
    /**
     * Set attribute: mProfileKeyID
     * 
     * @param ProfileKeyID [in] SafetyChannel<Integer>
     * 
     *          Data for assigning to attribute : mProfileKeyID
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     * @return None
     */    
    public void setProfileKeyID(SafetyChannel<Integer> ProfileKeyID)
    {
        this.mProfileKeyID = ProfileKeyID;
    }    
    
    /**
     * Return attribute: mEndTime
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Returned value of attribute : mEndTime 
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */    
    public SafetyChannel<Integer> getEndTime()
    {
        return mEndTime;
    }
    
    /**
     * Set attribute: mEndTime
     * 
     * @param EndTime [in] SafetyChannel<Integer>
     * 
     *          Data for assigning to attribute : mEndTime
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     *          
     * @return None
     */    
    public void setEndTime(SafetyChannel<Integer> EndTime)
    {
        this.mEndTime = EndTime;
    }  
       
    /**
     * Return attribute: mBasal
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Returned value of attribute : mBasal 
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */    
    public SafetyChannel<Integer> getBasal()
    {
        return mBasal;
    }
    
    /**
     * Set attribute: mBasal
     * 
     * @param Basal [in] SafetyChannel<Integer>
     * 
     *          Data for assigning to attribute : mBasal
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     *          
     * @return None
     */    
    public void setBasal(SafetyChannel<Integer> Basal)
    {
        this.mBasal = Basal;
    }    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// [New Feature] Basal Delivery comms functions
// [Update] Modify comments
// [Update] Add WaitDelivery for long time delivery time
