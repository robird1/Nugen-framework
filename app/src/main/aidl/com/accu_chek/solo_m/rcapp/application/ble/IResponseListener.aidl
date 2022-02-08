package com.accu_chek.solo_m.rcapp.application.ble;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;

/**
* The listener is designed for applications to listen responses they want.
*
*/

interface IResponseListener
{
	/**
	*
	*
	*/
	void onReceiveResponse(in ResponsePack pack);
}