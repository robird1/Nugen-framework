/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state
 * Brief: Abstract of BLEStateHandler
 *
 * Create Date: 2015/7/21
 * $Revision: 25032 $
 * $Author: KiddYeh $
 * $Id: AbstractBLEStateHandler.java 25032 2015-11-27 10:37:37Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public abstract class AbstractBLEStateHandler
{
    protected BLECallbackContext mCallbackContext = null;
    
    /**
     *  This method is an abstract function. 
     * 
     * @param callbackContext: the ble state handler
     *            Range: a valid object of BLECallbackContext
     *            Unit: BLECallbackContext
     *            Scaling: 1
     * 
     * @return void [out]
     */
    public AbstractBLEStateHandler(BLECallbackContext callbackContext)
    {
        mCallbackContext = callbackContext;
    }
    
    /**
     * This method is an abstract function. 
     *
     * @param request [in] the request    
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1  
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: SafetyByteArray
     *            Scaling: 1 
     *                      
     * @return void [out]
     * 
     */
    public void onRequestTimeout(IBLERequest request, BLERequestParameter parameter)
    {
        
    }
    
    /**
     * This method is an abstract function. 
     * 
     * @param isConnected [in] Connection State
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean.TRUE/FALSE
     *            Scaling: 1 
     *                      
     * @return void [out]
     * 
     */
    public void onConnectionStateChanged(int state)
    {
        
    }
     /**
     * This method is an abstract function. 
     * 
     * @param request: the current request
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1      
     * @param parameter: the parameter of the request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1    
     * @param cause: the cause of the response of the current request
     *            Range: -2^31 to (2^31)-1            
     *            Unit: integer
     *            Scaling: 1 
     *                      
     * @return void [out]
     * 
     */
    public void onWriteResponse(IBLERequest request, BLERequestParameter parameter, int cause)
    {
        
    }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */// (R15818 2015-08-31 04:11:59 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
// (R21192 2015-10-07 22:59:05 KiddYeh)
// ----------------------------------------------------------------------------
// [New Feature] Basal Delivery comms functions
