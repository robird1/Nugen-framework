/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: TbrData
 * Brief: Data storage of temporary basal rate  
 *
 * Create Date: 11/06/2015
 * $Revision: 25150 $
 * $Author: JacksonHuang $
 * $Id: TbrData.java 25150 2015-11-30 09:21:10Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms.database;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;

public class TbrData
{
    // Percentage
    private SafetyChannel<Integer> mPercentage = null;
    // Duration
    private SafetyChannel<Integer> mDuration = null;
    
    
    /**
     * Class Constructor
     * 
     */    
    public  TbrData()
    {
        
    }
            
    /**
     * Return attribute: mPercentage
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Returned value of attribute : mPercentage 
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */    
    public SafetyChannel<Integer> getPercentage()
    {
        return mPercentage;
    }
    
    /**
     * Set attribute: mPercentage
     * 
     * @param Percentage [in] SafetyChannel<Integer>
     * 
     *          Data for assigning to attribute : mPercentage
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     *          
     * @return None          
     */    
    public void setPercentage(SafetyChannel<Integer> Percentage)
    {
        this.mPercentage = Percentage;
    } 
    
    /**
     * Return attribute: mDuration
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Returned value of attribute : mDuration  
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */    
    public SafetyChannel<Integer> getDuration()
    {
        return mDuration;
    }
    
    /**
     * Set attribute: mDuration
     * 
     * @param Duration [in] SafetyChannel<Integer>
     * 
     *          Data for assigning to attribute : mDuration
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     *          
     * @return None
     */    
    public void setDuration(SafetyChannel<Integer> Duration)
    {
        this.mDuration = Duration;
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
