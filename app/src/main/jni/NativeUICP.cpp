//********** COPYRIGHT 2015 altek Corporation *********************************
// FILE NAME:
// VERSION:     $Revision: 19842 $
// DESCRIPTION:
//
//
//*****************************************************************************
// UPDATE LOG:
// $Log: $
//*****************************************************************************
//****************** STANDARD CPP-INCLUDE FILES *********************************
//************* GLOBAL AND PROJECT INCLUDE FILES ******************************
#define LOG_NDEBUG 0 //This macro is used to switch show/disable logcat
#define LOG_TAG "NativeUICP"

#include <NativeUICP.h>

/**
 * Tx mutex
 */
static pthread_mutex_t UICP_sendMutex;

/**
 * Rx mutex
 */
static pthread_mutex_t UICP_receiveMutex;

/**
 * Poll UICP value.
 * @param  expectStatus   [in] Excpect UICP value in UICP_FLOW_CONTROL.
 * Range: ACTIVE:0 or INACTIVE:1
 * Unit: N/A
 * Scaling: N/A
 * @param  pf   [in] function pointer
 * Range: UART_GetDSR or UART_GetCTS
 * Unit: N/A
 * Scaling: N/A
 * @return int [out] Return 0 if polling result is getting expected value.
 * Return -1 if fail to poll. Return -2 if polling result is not getting expected value.
 */
	int UART_PollingUICP(unsigned int expectStatus, int (NativeUICP::*pf)(unsigned int*))
	{
		int error = -1;
		const int STPO_POLLING = 55;
		unsigned int stoptime = UICP_AT_LEST_REACTION_MICROSECOND;
		int pollingtimes = 0;
		unsigned int status;
		int (NativeUICP::*pfunc)(unsigned int*) = pf;

		for(pollingtimes = 0; pollingtimes < UICP_POLLING_TIMES; pollingtimes++)
		{
			error = (NativeUICP::getInstance()->*pfunc)(&status);

			//ALOGD("Error= %d Polling a time index: %d status: %d", error, pollingtimes, status);

			if((error >= 0) && (status == expectStatus))
			{
				pollingtimes = UICP_POLLING_TIMES + STPO_POLLING;
			}
			else
			{
				usleep(stoptime);
			}
		}

		//ALOGD("pollingtimes= %d ", pollingtimes);

		if(pollingtimes == UICP_POLLING_TIMES)
		{
			if(error == 0)
			{
				// result is not expected
				error = -2;
			}
			else
			{
				error = -1;
			}
		}
		else
		{
			// succeed read expected value
			error = 0;
		}

		return error;
	}

/**
 * NativeUICP Constructor initialize the mutex of UICP Tx and Rx here, but
 * protecting the constructor
 * meets Sigleton pattern.
 * 
 * @param None    [in]
 */
    NativeUICP::NativeUICP()
    {
    	ALOGD("NativeUICP constructor");

    	pthread_mutex_init(&UICP_sendMutex, NULL);
    	pthread_mutex_init(&UICP_receiveMutex, NULL);
    }

/**
 * NativeUICP Destructor that does nothing here.
 * 
 * @param None    [in]
 * @return None [out]
 */
    NativeUICP::~NativeUICP()
    {

    }

/**
 * Get NativeUICP instance for implementing Singleton pattern.
 * 
 * @param None    [in]
 * @return None [out]
 */
    NativeUICP* NativeUICP::getInstance()
    {
    	static NativeUICP *pInstance = new NativeUICP;

    	return pInstance;
    }

/**
 * Reset UICP
 * 
 * @param None [in]
 * @return None [out]
 */
	void NativeUICP::UART_ResetUICP(void)
	{
		ALOGI("UART_ResetUICP");

		pthread_mutex_lock(&UICP_sendMutex);

		UART_SetDTR(INACTIVE);
        UART_SetRTS(INACTIVE);

		pthread_mutex_unlock(&UICP_sendMutex);

	}

/**
 * Open UICP Tx according to the rule of UICP+.
 * 1. Activate DTR.
 * 2. Polling the status of CTS (max time is one second)
 * 3. If CTS is active, the action of UICP Tx is correct.
 * Otherwise, set DTR INACTIVE and stopping steps.
 * 
 * @param None [in]
 * @return int [out] Return open UICP Tx result.
 * Range:
 * 0: opening Tx succeeds.
 * -1: fail to poll or fail to open Tx.
 * -2: polling result is not getting expected value.
 * Unit: N/A
 * Scaling: N/A
 */
	int NativeUICP::UART_OpenUICPTx()
	{
		int error = -1;

		ALOGI("UART_OpenUICPTx");

		pthread_mutex_lock(&UICP_sendMutex);

		UART_SetDTR(ACTIVE);

		// wait CTS 1s at most
		error = UART_PollingUICP(ACTIVE, &NativeUICP::UART_GetCTS);

		// recovery strategy: interface down
		if(error >= 0)
		{
			// Tx open succeed
			error = 0;
		}
		else
		{
			// view CTS INACTIVE
			// continue normal UICP
			UART_SetDTR(INACTIVE);
			ALOGI("UART_OpenUICPTx:CTS is INACTIVE and fail to open Tx, but UICP works normally");
		}

		pthread_mutex_unlock(&UICP_sendMutex);

		return error;
	}


/**
 * Close UICP Tx according to the rule of UICP+.
 * 1. Stop sending in 100ms.
 * 2. Polling the status of CTS (max time is one second)
 * 3. If CTS is active, the action of UICP Tx is correct.
 * Otherwise, Stopping steps.
 * 4. If CTS is correct, set DTR INACTIVE.
 * 5. Polling the status of CTS (max time is one second)
 * 6. If CTS is INACTIVE, the action of closing UICP Tx is correct.
 * Otherwise, set DTR ACTIVE and stopping steps.
 * 
 * @param None [in]
 * @return int [out] Return close UICP Tx result.
 * Range:
 * 0: closing Tx succeeds.
 * -1: fail to poll.
 * -2: polling result is not getting expected value.
 * Unit: N/A
 * Scaling: N/A
 */
	int NativeUICP::UART_CloseUICPTx()
	{
		int error = -1;
		unsigned int stoptime;

		ALOGI("UART_CloseUICPTx");
		pthread_mutex_lock(&UICP_sendMutex);

		// stop send 100ms
		stoptime = UICP_WAIT_TX_FINISH_MICROSECOND;

		usleep(stoptime);

		// read CTS and value is ACTIVE, wait CTS 1s at most
		error = UART_PollingUICP(ACTIVE, &NativeUICP::UART_GetCTS);

		if(error >= 0)
		{
			int pollingtimes = 0;
			const int GET_INACTIVE = 55;

			UART_SetDTR(INACTIVE);

			// wait CTS 1s at most
			error = UART_PollingUICP(INACTIVE, &NativeUICP::UART_GetCTS);

			// recovery strategy: interface up
			if(error >= 0)
			{
				// Tx close succeed
				error = 0;
			}
			else
			{
				// view CTS ACTIVE
				UART_SetDTR(ACTIVE);
				ALOGI("UART_CloseUICPTx: CTS = ACTIVE, but UICP works normally");
			}
		}
		else
		{
			// error flow
			// the IUC signal of the RIUA shall be active
			ALOGI("UART_CloseUICPTx: CTS SHALL be active THEN close UICP Tx, but CTS is INACTIVE.");
		}

		pthread_mutex_unlock(&UICP_sendMutex);

		return error;
	}

/**
 * Open UICP Rx according to the rule of UICP+.
 * 1. Polling the status of DSR (max time is one second)
 * 2. If DSR is active, the action of UICP Rx is correct.
 * Otherwise, Stopping steps.
 * 3. If DSR is correct, set RTS ACTIVE.
 * 
 * @param None [in]
 * @return int [out] Return open UICP Rx result.
 * Range:
 * 0: opening Tx succeeds.
 * -1: fail to poll.
 * -2: polling result is not getting expected value.
 * Unit: N/A
 * Scaling: N/A
 */
	int NativeUICP::UART_OpenUICPRx()
	{
		int error = -1;
		unsigned int flowStatus;

		ALOGD("UART_OpenUICPRx");
		pthread_mutex_lock(&UICP_receiveMutex);

		// wait DSR 1s at most
		error = UART_PollingUICP(ACTIVE, &NativeUICP::UART_GetDSR);

		//ALOGD("UART_OpenUICPRx:PollingUICP error=%d, expected=%d", error, ACTIVE);

        UART_SetRTS(ACTIVE);
		if(error >= 0)
		{
			//UART_SetRTS(ACTIVE);
		}
		else
		{
            error = 0;
			// error or polling result is inactive
			ALOGE("Open Rx Error =========================");
		}

		pthread_mutex_unlock(&UICP_receiveMutex);

		return error;
	}

/**
 * Close UICP Rx according to the rule of UICP+.
 * 1. Polling the status of DSR (max time is one second)
 * 2. If DSR is active, the action of UICP Rx is correct.
 * Otherwise, stopping steps.
 * 3. If DSR is correct, set RTS INACTIVE
 * 
 * @param None [in]
 * @return int [out] Return close UICP Rx result.
 * Range:
 * 0: closing Rx succeeds.
 * -1: fail to poll.
 * -2: polling result is not getting expected value.
 * Unit: N/A
 * Scaling: N/A
 */
	int NativeUICP::UART_CloseUICPRx()
	{
		int error = -1;
		unsigned int flowStatus;

		pthread_mutex_lock(&UICP_receiveMutex);

		ALOGD("UART_CloseUICPRx");
		// wait DSR 1s at most
		error = UART_PollingUICP(INACTIVE, &NativeUICP::UART_GetDSR);
		//error = 0;

		//ALOGD("Polling result close uicp rx error:%d expect status(INACTIVE/1 ACTIVE/0): %d", error, INACTIVE);

		if(error >= 0)
		{
			UART_SetRTS(INACTIVE);

		}
		else
		{
			ALOGE("Close Rx Error =========================");
			// return error value
			// -1: error
			// -2: unexpected value
		}

		pthread_mutex_unlock(&UICP_receiveMutex);

		return error;
	}

	int NativeUICP::UART_GetCOMMSSendingStatus(int* shouldStop)
	{
		int error = -1;
		unsigned int status = INACTIVE;

		error = UART_GetDSR(&status);
		//error = UART_PollingUICP(INACTIVE, &NativeUICP::UART_GetDSR);
		ALOGE("UART_GetCOMMSSendingStatus:UART_GetDSR(INACTIVE)=%d", error);
		if (status == INACTIVE)
		{
			*shouldStop = REMOTE_STOP;
		}
		else
		{
			*shouldStop = REMOTE_SEND;
		}

		return error;
	}

/**
 * Set UICP as function mode.
 * 
 */
	void NativeUICP::UART_SetUICPFunctionMode()
	{
		CUSTHAL *halOperation = CUSTHAL::getInstance();

		halOperation->setFunctionMode();
	}


/**
 * Get DSR status.
 * 
 * @return int [out] If the getting succeed, return 0. Otherwise, return -1.
 * Range:
 * 0: getting succeed
 * -1: fail
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param status    [out] the status of DSR.
 * Range:
 * 0: ACTIVE
 * 1: INACTIVE
 * Unit: N/A
 * Scaling: N/A
 */
	int NativeUICP::UART_GetDSR(unsigned int *status)
	{
		CUSTHAL *halOperation = CUSTHAL::getInstance();
		int error = -1;

		error = halOperation->getDSR(status);

		return error;
	}

/**
 * Get CTS status.
 * 
 * @return int [out] If the getting succeed, return 0. Otherwise, return -1.
 * Range:
 * 0: getting succeed
 * -1: fail
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param status    [out] the status of CTS.
 * Range:
 * 0: ACTIVE
 * 1: INACTIVE
 * Unit: N/A
 * Scaling: N/A
 */
	int NativeUICP::UART_GetCTS(unsigned int *status)
	{
		CUSTHAL *halOperation = CUSTHAL::getInstance();
		int error = -1;

		error = halOperation->getCTS(status);

		ALOGD("UART_GetCTS: error=%d, status=%d", error, *status);

		return error;
	}

/**
 * Set RTS status.
 * 
 * @return None [out]
 * 
 * @param status    [in] the status of RTS.
 * Range:
 * 0: ACTIVE
 * 1: INACTIVE
 * Unit: N/A
 * Scaling: N/A
 */
	void NativeUICP::UART_SetRTS(int status)
	{
		CUSTHAL *halOperation = CUSTHAL::getInstance();
//		int error = -1;

		halOperation->setGPIOMode();
		halOperation->setRTS(status);
		//halOperation->setFunctionMode();



//		return error;
	}

/**
 * Set DTR status.
 * 
 * @return None [out]
 * 
 * @param status    [in] the status of DTR.
 * Range:
 * 0: ACTIVE
 * 1: INACTIVE
 * Unit: N/A
 * Scaling: N/A
 */
	void NativeUICP::UART_SetDTR(UICP_FLOW_CONTROL status)
	{
		CUSTHAL *halOperation = CUSTHAL::getInstance();
//		int error = -1;

		halOperation->setGPIOMode();
		halOperation->setDTR(status);
		//halOperation->setFunctionMode();

//		ALOGE("uicp port dtr: %d ", error);

//		return error;

	}

