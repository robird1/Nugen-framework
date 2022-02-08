package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBGMOnListener;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IMeInformationListener;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

interface IBGMControl
{
   
    void initBgm();
 
 	void wakeUpBgm();
  	
  	void setOnListener(IBGMOnListener listener);
  
	void getMeInformation(String action, IMeInformationListener MeInformationlistener);
	
	void updateCodeKey(in SafetyByteArray codeKeyData,IMeInformationListener MeInformationlistener);
	 }