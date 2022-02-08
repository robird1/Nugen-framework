package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
 
/**
 * {@hide}
 */
interface IBGMOnListener
{
    //call back function
    void onProcessing();
    void onBlood();
    void onStripInsert();
    void onWaitStrip();
    void onRemoveEarly();
    void onbgResult();
    void oncgResult( boolean L1, boolean L2);
    void onFATStrip();
    void onCheckWorkFlow();
    void onDetectedStrip();
	void onUsbConnect();
    void onStartUp();
}