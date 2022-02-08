package com.accu_chek.solo_m.rcapp.application.emwrservice;

import com.accu_chek.solo_m.rcapp.application.emwrservice.IEMWRInformation;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

interface IEMWRService 
{
    /**
     * Call EMWR service to show EMWR message in EMWR service. 
     * 
     * @param storageId message id in MessageLog table
     * Range: valid SafetyString object
     * Unit: SafetyString
     * Scaling: 1
     * @param message IEMWRInformation that includes message item, sub content (option) and callback (option).
     * Range: valid IEMWRInformation
     * Unit: IEMWRInformation
     * Scaling: 1
     * 
     * return None
     */
    void requireEMWR(in SafetyString storageId, IEMWRInformation message);
    
    /**
     * Show EMWR message that has been in EMWR queue.
     *
     * return None
     */
    void repeatEMWR();  
    
    /**
     * Put unconfirmed EMWR messages and EMWR list into EMWR queue.
     *
     * return None
     */
    void putHistory(); 
}