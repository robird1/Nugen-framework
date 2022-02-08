//********** COPYRIGHT 2013 altek Corporation *********************************
// FILE NAME:   This is template file.
// VERSION:     $Revision: 19842 $
// DESCRIPTION: The file is for java framework service jni
// 
//
//*****************************************************************************
// UPDATE LOG:
// $Log: $
//*****************************************************************************
//****************** STANDARD CPP-INCLUDE FILES *********************************
//************* GLOBAL AND PROJECT INCLUDE FILES ******************************
#define LOG_NDEBUG 0 //This macro is used to switch show/disable logcat
#define LOG_TAG "UICP_JNI"

#include <jni.h>
#include <custHal.h>
#include <NativeUART.h>
#include <NativeUICP.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <android/log.h>

// Log functions
#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )

//This is java full name qualifier, use to assign java servcie framework
static const char* const kClassName = "com/accu_chek/solo_m/rcapp/communication/uartuicp/UICP";

/**
 * Throw Fail Operation Exception to JAVA layer.
 * 
 * 
 * return None
 * throw OperationFailException if UART operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param message    [in] const char format of a string message in the exception.
 * char string constraints:
 * Range: Valid const char pointer
 * Unit: const char pointer
 * Scaling: 1
 */
static int UART_ThrowFailOperationException(JNIEnv *env, const char *message)
{
    jclass exceptionClass;
    const char* className = "com/accu_chek/solo_m/rcapp/application/exception/OperationFailException";
    const char* notFoundClassName = "java/lang/ClassNotFoundException";

    ALOGD("Throw Exception");

    exceptionClass = env->FindClass(className);

    if (exceptionClass == NULL)
    {
    	ALOGE("Can't find OperationfailException");

    	className = notFoundClassName;
    	message = "Exception OperationFailException is not found.";
    }

    return env->ThrowNew(exceptionClass, message);
}

/**
 * Reset UICP
 * 
 * 
 * return None
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * object provided by Android SDK.
 */
void UART_ResetUICP(JNIEnv *env, jclass clazz)
{
	NativeUICP *uicpControl = NativeUICP::getInstance();

	uicpControl->UART_ResetUICP();
}

/**
 * Open UICP Tx.
 * 
 * 
 * return None
 * throw OperationFailException if UART operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * object provided by Android SDK.
 */
void UART_OpenUICPTx(JNIEnv *env, jclass clazz)
{
	int error = -1;
	NativeUICP *uicpControl = NativeUICP::getInstance();

	error = uicpControl->UART_OpenUICPTx();

	if(error == -2)
	{
		ALOGD("UART_OpenUICPTx: COMMS not confirm!");
		UART_ThrowFailOperationException(env, "Fail to open because do not get COMMS confirm.");
	}
	else if(error < 0)
	{
		ALOGD("UART_OpenUICPTx: Error");
		UART_ThrowFailOperationException(env, "Open UICP tx error.");
	}
}

/**
 * Close UICP Tx.
 * 
 * 
 * return None
 * throw OperationFailException if UART operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * object provided by Android SDK.
 */
void UART_CloseUICPTx(JNIEnv *env, jclass clazz)
{
	int error = -1;
	NativeUICP *uicpControl = NativeUICP::getInstance();

	error = uicpControl->UART_CloseUICPTx();

	if(error < 0)
	{
		ALOGD("UART_CloseUICPTx: error");
		UART_ThrowFailOperationException(env, "Close UICP tx error.");
	}

}

/**
 * Open UICP Rx.
 * 
 * 
 * return None
 * throw OperationFailException if UART operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * object provided by Android SDK.
 */
void UART_OpenUICPRx(JNIEnv *env, jclass clazz)
{
	int error = -1;
	NativeUICP *uicpControl = NativeUICP::getInstance();

	error = uicpControl->UART_OpenUICPRx();

	if(error < 0)
	{
		ALOGD("UART_OpenUICPRx: error");
		UART_ThrowFailOperationException(env, "Open UICP Rx error.");
	}

}

/**
 * Close UICP Rx.
 * 
 * 
 * return None
 * throw OperationFailException if UART operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * object provided by Android SDK.
 */
void UART_CloseUICPRx(JNIEnv *env, jclass clazz)
{
	int error = -1;
	NativeUICP *uicpControl = NativeUICP::getInstance();

	error = uicpControl->UART_CloseUICPRx();

	if(error < 0)
	{
		ALOGD("UART_CloseUICPRx: error");
		UART_ThrowFailOperationException(env, "Close UICP Rx error.");
	}
}

/**
 * Get COMMS Sending Status.
 *
 * return jint Return REMOTE_STOP if COMMS stops sending. Otherwise, return REMOTE_SEND. The returned value is in UICP_FLOW_STATUS.
 * throw OperationFailException if UART operation fails.
 * throw ClassNotFoundException if exception class is not found.
 *
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * object provided by Android SDK.
 */
jint UART_GetCOMMSSendingStatus(JNIEnv *env, jclass clazz)
{
	int error = -1;
	int shouldStop = -1;
	NativeUICP *uicpControl = NativeUICP::getInstance();

	error = uicpControl->UART_GetCOMMSSendingStatus(&shouldStop);

	if(error < 0)
	{
		ALOGD("UART_GetCOMMSSendingStatus: error");
		UART_ThrowFailOperationException(env, "Fail to get remote's status.");
	}

	return shouldStop;
}

/**
 * Set UICP as function mode.
 */
void UART_SetUICPFunctionMode(JNIEnv *env, jclass clazz)
{
	NativeUICP *uicpControl = NativeUICP::getInstance();

	uicpControl->UART_SetUICPFunctionMode();
}


//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] = {
    {"resetUICP",     "()V", (void*) UART_ResetUICP },
    {"openUICPTx",     "()V", (void*) UART_OpenUICPTx },
    {"closeUICPTx",     "()V", (void*) UART_CloseUICPTx },
    {"openUICPRx",     "()V", (void*) UART_OpenUICPRx },
    {"closeUICPRx",     "()V", (void*) UART_CloseUICPRx },
    {"getCOMMSSendingStatus",     "()I", (void*) UART_GetCOMMSSendingStatus },
    {"setUICPFunctionMode",     "()V", (void*) UART_SetUICPFunctionMode },
};


/**
 * Register JNINativeMethod function table to DVM.
 * 
 * return integer number. If registering succeeds, return 0. Otherwise, return -1.
 * 
 * Range:
 * 0: succeed to register UICP
 * -1: fail to register UICP
 * Unit: int
 * Scaling: 1
 * 
 * @param env    [in] JNIEnv is jni environment pointer. JNIEnv object is provided
 * by Android SDK.
 */
int register_custinterface_customizedhwmanager_uicp(JNIEnv* env)
{

    jclass clazz;
    
    /* look up the class */
    clazz = env->FindClass(kClassName);
    if (clazz == NULL) {
        ALOGE("Can't find class %s\n", kClassName);
        return -1;
    }
    
    /* register all the methods */
    if (env->RegisterNatives(clazz, gMethods,
            sizeof(gMethods) / sizeof(gMethods[0])) != JNI_OK)
    {
        ALOGE("Failed registering methods for %s\n", kClassName);
        return -1;
    }

    /* fill out the rest of the ID cache */
    return 0;
}
