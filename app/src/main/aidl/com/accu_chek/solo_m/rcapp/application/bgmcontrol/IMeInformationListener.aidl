package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
/**
 * {@hide}
 */
interface IMeInformationListener
{
	void onCodeKeyInformation(in SafetyByteArray number, in SafetyByteArray date, in SafetyByteArray status, in SafetyByteArray field);
 	void onStripCounter(in SafetyByteArray counter);
 	void onSuccess(in SafetyString key, in SafetyString result);
 	void onError();
}