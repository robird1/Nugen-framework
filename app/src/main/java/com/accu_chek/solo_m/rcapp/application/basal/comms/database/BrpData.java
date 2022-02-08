/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BrpData
 * Brief: Storage of Basal Rate Profile Data
 *
 * Create Date: 11/06/2015
 * $Revision: 24156 $
 * $Author: JacksonHuang $
 * $Id: BrpData.java 24156 2015-11-16 05:50:26Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms.database;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

public class BrpData
{
    // Database Key ID of this basal rate profile
    private SafetyChannel<Integer> mProfileKeyID = null;
    // Name of this basal rate profile
    private SafetyString mProfileName = null;
    // Activation status of this basal rate profile
    private SafetyBoolean mActivated = null;
    // Sum of units of this basal rate profile
    private SafetyChannel<Integer> mTotalBasal = null;
    // Ordering information of this basal rate profile
    private SafetyChannel<Integer> mOrderingInfo = null;
    
    /**
     * Class Constructor
     * 
     */    
    public  BrpData()
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
     * Return attribute: mProfileName
     * 
     * @return SafetyString [out]
     * 
     *          Returned value of attribute : mProfileName  
     * 
     *          Range: Valid SafetyString object
     *          Unit: SafetyString
     *          Scaling: 1
     */    
    public SafetyString getProfileName()
    {
        return mProfileName;
    }
    
    /**
     * Set attribute: mProfileName
     * 
     * @param ProfileName [in] SafetyString
     * 
     *          Data for assigning to attribute : mProfileName
     * 
     *          Range: Valid SafetyString object
     *          Unit: SafetyString
     *          Scaling: 1
     *          
     * @return None
     */    
    public void setProfileName(SafetyString ProfileName)
    {
        this.mProfileName = ProfileName;
    }    
    
    /**
     * Return attribute: mActivated
     * 
     * @return SafetyBoolean [out]
     * 
     *          Returned value of attribute : mActivated 
     * 
     *          Range: Valid SafetyBoolean object
     *          Unit: SafetyBoolean
     *          Scaling: 1
     */    
    public SafetyBoolean getActivated()
    {
        return mActivated;
    }
    
    /**
     * Set attribute: mActivated
     * 
     * @param Activated [in] SafetyBoolean
     * 
     *          Data for assigning to attribute : mActivated
     * 
     *          Range: Valid SafetyBoolean object
     *          Unit: SafetyBoolean
     *          Scaling: 1
     *          
     * @return None
     */    
    public void setActivated(SafetyBoolean Activated)
    {
        this.mActivated = Activated;
    }    
     
    /**
     * Return attribute: mTotalBasal
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Returned value of attribute : mTotalBasal
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */    
    public SafetyChannel<Integer> getTotalBasal()
    {
        return mTotalBasal;
    }
    
    /**
     * Set attribute: mTotalBasal
     * 
     * @param TotalBasal [in] SafetyChannel<Integer>
     * 
     *          Data for assigning to attribute : mTotalBasal
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     *          
     * @return None
     */    
    public void setTotalBasal(SafetyChannel<Integer> TotalBasal)
    {
        this.mTotalBasal = TotalBasal;
    }    
    
    /**
     * Return attribute: mOrderingInfo
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Returned value of attribute : mOrderingInfo
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     */    
    public SafetyChannel<Integer> getOrderingInfo()
    {
        return mOrderingInfo;
    }    
    
    /**
     * Set attribute: mOrderingInfo
     * 
     * @param OrderingInfo [in] SafetyChannel<Integer>
     * 
     *          Data for assigning to attribute : mOrderingInfo
     * 
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     *          
     * @return None
     */    
    public void setOrderingInfo(SafetyChannel<Integer> OrderingInfo)
    {
        this.mOrderingInfo = OrderingInfo;
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
