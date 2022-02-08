#ifndef UART_UICP_H
#define UART_UICP_H

#include <jni.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <custHal.h>
#include <time.h>

#include <android/log.h>

// Log functions
#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )

// waiting period time for reading/writing
#define UICP_DETECT_TIMING_MICROSECOND (50*1000)

// longest time to wait reply
#define UICP_LOST_SIGNAL_MICROSECOND (1000*1000)

// shortest time to wait reply
#define UICP_AT_LEST_REACTION_MICROSECOND (50*1000)

// wait tx finish
#define UICP_WAIT_TX_FINISH_MICROSECOND (100*1000)

// polling times in one second
#define UICP_POLLING_TIMES (UICP_LOST_SIGNAL_MICROSECOND/UICP_DETECT_TIMING_MICROSECOND)

// the type of UICP flow control
// ACTIVE means starting action
// INACTIVE means stopping action
typedef enum UICP_FLOW_CONTROL
{
	ACTIVE = 0,
	INACTIVE = 1
}UICP_FLOW_CONTROL;

typedef enum UICP_FLOW_STATUS
{
	REMOTE_STOP = 0x000F,
	REMOTE_SEND = 0x0033
}UICP_FLOW_STATUS;

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
class NativeUICP;
int UART_PollingUICP(unsigned int expectStatus, int (NativeUICP::*pf)(unsigned int*));

class NativeUICP
{
	public:
		/**
		 * NativeUICP Destructor that does nothing here.
		 *
		 * @param None    [in]
		 * @return None [out]
		 */
		virtual ~NativeUICP();

		/**
		 * Get NativeUICP instance for implementing Singleton pattern.
		 *
		 * @param None    [in]
		 * @return None [out]
		 */
		static NativeUICP* getInstance();

		/**
		 * Reset UICP
		 *
		 * @param None [in]
		 * @return None [out]
		 */
		void UART_ResetUICP(void);

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
		int UART_OpenUICPTx(void);

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
		int UART_CloseUICPTx(void);

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
		int UART_OpenUICPRx(void);

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
		int UART_CloseUICPRx(void);

		/**
		 * Get COMMS Sending Status.
		 *
		 * @return jint Return REMOTE_STOP if COMMS stops sending. Otherwise, return REMOTE_SEND. The returned value is in UICP_FLOW_STATUS.
		 * Range:
		 * REMOTE_STOP: COMMS stops sending.
		 * REMOTE_SEND: COMMS still sends.
		 * Unit: N/A
		 * Scaling: N/A		 
		 * @throw OperationFailException if UART operation fails.
		 * @throw ClassNotFoundException if exception class is not found.
		 *
		 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
		 * object is provided by Android SDK.
		 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
		 * object provided by Android SDK.
		 */
		int UART_GetCOMMSSendingStatus(int* status);

        /**
         * Set UICP as function mode.
         */
    	void UART_SetUICPFunctionMode();

	private:

		/**
		 * NativeUICP Constructor initialize the mutex of UICP Tx and Rx here, but protecting the constructor
		 * meets Sigleton pattern.
		 *
		 * @param None    [in]
		 */
		NativeUICP();

		/**
		 * NativeUICP copy does nothing here, but protecting the function
		 * meets Sigleton pattern.
		 *
		 * @param uicpObject    [in] NativeUICP object that is used to copy.
		 */
//		NativeUICP(const NativeUICP&);

		/**
		 * NativeUICP copy does nothing here, but protecting the function
		 * meets Sigleton pattern.
		 *
		 * @param uicpObject    [in] NativeUICP object that is used to operate.
		 * @return NativeUICP [out] Return operation result.
		 */
//		const NativeUICP& operator= (const NativeUICP&);

		/**
		 * Get DSR status.
		 *
		 * @param status [out] the status of DSR.
		 * Range:
		 * 0: ACTIVE
		 * 1: INACTIVE
		 * Unit: N/A
		 * Scaling: N/A
		 * @return int [out] If the getting succeed, return 0. Otherwise, return -1.
		 * Range:
		 * 0: getting succeed
		 * -1: fail
		 * Unit: N/A
		 * Scaling: N/A
		 */
		int UART_GetDSR(unsigned int *status);

		/**
		 * Get CTS status.
		 *
		 * @param status [out] the status of CTS.
		 * Range:
		 * 0: ACTIVE
		 * 1: INACTIVE
		 * Unit: N/A
		 * Scaling: N/A
		 * @return int [out] If the getting succeed, return 0. Otherwise, return -1.
		 * Range:
		 * 0: getting succeed
		 * -1: fail
		 * Unit: N/A
		 * Scaling: N/A
		 */
		int UART_GetCTS(unsigned int *status);

		/**
		 * Set RTS status.
		 *
		 * @param status [in] the status of RTS.
		 * Range:
		 * 0: ACTIVE
		 * 1: INACTIVE
		 * Unit: N/A
		 * Scaling: N/A
		 * @return None [out]
		 */
		void UART_SetRTS(int status);

		/**
		 * Set DTR status.
		 *
		 * @param status [in] the status of DTR.
		 * Range:
		 * 0: ACTIVE
		 * 1: INACTIVE
		 * Unit: N/A
		 * Scaling: N/A
		 * @return None [out]
		 */
		void UART_SetDTR(UICP_FLOW_CONTROL status);
};
#endif //UART_UICP_H
