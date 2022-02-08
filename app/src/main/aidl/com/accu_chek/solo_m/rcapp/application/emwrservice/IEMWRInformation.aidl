package com.accu_chek.solo_m.rcapp.application.emwrservice;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

interface IEMWRInformation 
{
    /**
     * Get a string that meets JSONObject format.
     *
     * return SafetyString that includes message name, the callback status of the message and sub content.
     * Range: valid SafetyString object.
     * Unit: SafetyString
     * Scaling: 1
     */
    SafetyString getMessageInfoString();
    /**
     * Execute the callback of left button.
     *
     * see mLeftCallback [in] This global variable is referred for getting the callback of left button.
     *
     * return None
     */
    void executeLeftButton();
    /**
     * Execute the callback of right button.
     *
     * see mRightCallback [in] This global variable is referred for getting the callback of right button.
     * 
     * return None
     */
    void executeRightButton();
    /**
     * Execute the callback of center button.
     *
     * see mCenterCallback [in] This global variable is referred for getting the callback of center button.
     *
     * return None
     */
    void executeCenterButton();
}