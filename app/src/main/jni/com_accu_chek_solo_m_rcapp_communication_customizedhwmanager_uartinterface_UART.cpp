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
#define LOG_TAG "UART_jni"

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

enum UART_ee_jniFDIndex
{
	// the COMMS uart fd index in file descriptor array
	COMMS_FD_INDEX = 0,

	// the BGM uart fd index in file descriptor array
	BGM_FD_INDEX
};

enum UART_ee_jniDeviceIndex
{
	// the COMMS index
	COMMS_INDEX = 0x000F,

	// the BGM index
	BGM_INDEX = 0x0033
};

//This is java full name qualifier, use to assign java servcie framework
static const char* const kClassName = "com/accu_chek/solo_m/rcapp/communication/customizedhwmanager/uartinterface/UART";

// file descriptor array to record fd of BLE port or COMMS port
static volatile int mFd[2] = {-1, -1};

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

    ALOGE("Throw Exception");

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
 * Set necessary information of a UART port, COMMS UART or BGM UART.
 * 1. the place in array that is stored file descriptor of a port
 * 2. the index of the port in NativeUART of native layer
 * 
 * 
 * return int [out] Return 0 if the port is correct. Otherwise, return -1.
 * Range:
 * 0: port range is correct,
 * -1: port is incorrect
 * Unit: int
 * Scaling: 1
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param port    [in] int index of UART port that is in UART_ee_jniDeviceIndex
 * Range: (in UART_ee_jniDeviceIndex)
 * COMMS_INDEX = 0x000F,
 * BGM_INDEX = 0x0033
 * Unit: int
 * Scaling: 1
 * @param portIndex    [out] the index of the port in NativeUART of native layer
 * in UART_ee_jniDeviceIndex
 * Range: (in UART_ee_jniDeviceIndex)
 * COMMS_INDEX = 0x000F,
 * BGM_INDEX = 0x0033
 * Unit: int pointer
 * Scaling: 1
 * @param fdIndex    [out] int index of the place in array that is stored file
 * descriptor of a port in UART_ee_jniFDIndex
 * Range: (in UART_ee_jniFDIndex)
 * COMMS_FD_INDEX = 0,
 * BGM_FD_INDEX = 1
 * Unit: int pointer
 * Scaling: 1
 */
static int UART_SetPortConfig(JNIEnv *env, int port, int *portIndex, int *fdIndex)
{
	int error = 0;

	switch(port)
	{
	case COMMS_INDEX:
		*portIndex = COMMS_INDEX;
		*fdIndex = COMMS_FD_INDEX;

		break;

	case BGM_INDEX:
		*portIndex = BGM_INDEX;
		*fdIndex = BGM_FD_INDEX;

		break;

	default:
		error = -1;
	}

	return error;
}

/**
 * Open UART port, COMMS UART or BGM UART.
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
 * @param port    [in] jint index of UART port that is in UART_ee_jniDeviceIndex
 * Range: (in UART_ee_jniDeviceIndex)
 * COMMS_INDEX = 0x000F,
 * BGM_INDEX = 0x0033
 * Unit: jint
 * Scaling: 1
 */
void UART_Open(JNIEnv *env, jclass clazz, jint port)
{
	int error = -1;

	int portIndex = -1;
	int fdIndex = -1;
	int fd = -1;
	NativeUART *native = NativeUART::getInstance();

	error = UART_SetPortConfig(env, port, &portIndex, &fdIndex);

	if(error >= 0)
	{
		if(mFd[fdIndex] == -1)
		{
			error = native->UART_Open(portIndex, &fd);

			if(error >= 0)
			{
				mFd[fdIndex] = fd;

				ALOGD("uart port first time: %d %d %d opened", fdIndex, mFd[fdIndex], error);
			}
			else
			{
				UART_ThrowFailOperationException(env, "Open UART port fails.");
			}
		}
		else
		{
			// Already opened
			ALOGD("uart port: %d %d opened", mFd[fdIndex], error);
		}
	}
	else
	{
		UART_ThrowFailOperationException(env, "Port configure setting is error.");
	}


	ALOGD("uart fd: %d uart error: %d  uart fd array index: %d", mFd[fdIndex], error,fdIndex);
}

/**
 * Close UART port, COMMS UART or BGM UART.
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
 * @param port    [in] jint index of UART port that is in UART_ee_jniDeviceIndex
 * Range: (in UART_ee_jniDeviceIndex)
 * COMMS_INDEX = 0x000F,
 * BGM_INDEX = 0x0033
 * Unit: jint
 * Scaling: 1
 */
void UART_Close(JNIEnv *env, jclass clazz, jint port)
{
	int error = -1;
	int portIndex = -1;
	int fdIndex = -1;
	int fd = -1;
	NativeUART *native = NativeUART::getInstance();

	error = UART_SetPortConfig(env, port, &portIndex, &fdIndex);

	if(error >= 0)
	{
		fd = mFd[fdIndex];

		if(fd != -1)
		{
			error = native->UART_Close(fd);

			if(error >= 0)
			{
				mFd[fdIndex] = -1;
			}
			else
			{
				mFd[fdIndex] = -1;
				UART_ThrowFailOperationException(env, "Fail to close UART port.");
			}
		}
		else
		{
			// Ignore
		}

		ALOGD("Closing succeed %d", mFd[fdIndex]);

	}
	else
	{
		UART_ThrowFailOperationException(env, "Port configure setting is error.");
	}
}

/**
 * Write data into a UART port, COMMS UART or BGM UART.
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
 * @param port    [in] jint index of UART port that is in UART_ee_jniDeviceIndex
 * Range: (in UART_ee_jniDeviceIndex)
 * COMMS_INDEX = 0x000F,
 * BGM_INDEX = 0x0033
 * Unit: jint
 * Scaling: 1
 * @param data    [in] jbyteArray format of sending data
 * Range: Valid jbyteArray
 * Unit: jbyteArray
 * Scaling: 1
 */
void UART_Write(JNIEnv *env, jclass clazz, jint port, jbyteArray data)
{
	int error = -1;
	int portIndex = -1;
	int fdIndex = -1;

	error = UART_SetPortConfig(env, port, &portIndex, &fdIndex);

	if(error >= 0)
	{
		if(mFd[fdIndex] != -1)
		{
			NativeUART *uartControl = NativeUART::getInstance();
			int bufferLength = env->GetArrayLength(data);
			jbyte* buffer = env->GetByteArrayElements(data,0);
			unsigned char* buffer_native = (unsigned char*)buffer;

			error = uartControl->UART_Write(mFd[fdIndex], bufferLength, buffer_native);
			env->ReleaseByteArrayElements(data, buffer,JNI_ABORT);
			if(error >= 0)
			{
				ALOGD("Sending succeed %d", mFd[fdIndex]);
			}
			else
			{
				// fail to write
				UART_ThrowFailOperationException(env, "uart writing fails");
			}
		}
		else
		{
			// port is not opened
			UART_ThrowFailOperationException(env, "Port is not opened.");
		}
	}
	else
	{
		// error
		UART_ThrowFailOperationException(env, "Port configure setting is error.");
	}

}

/**
 * Read data from a UART port, COMMS UART or BGM UART.
 * 
 * 
 * return integer nuber that is the length of received data
 * Range: 0 .. 512
 * Unit: int
 * Scaling: 1
 * throw OperationFailException if UART operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * object provided by Android SDK.
 * @param port    [in] jint index of UART port that is in UART_ee_jniDeviceIndex
 * Range: (in UART_ee_jniDeviceIndex)
 * COMMS_INDEX = 0x000F,
 * BGM_INDEX = 0x0033
 * Unit: jint
 * Scaling: 1
 * @param data    [in] jbyteArray format of receiving buffer
 * byte array constraints:
 * Range: 1 ... 512 (max size of byte array)
 * Unit: byte
 * Scaling: 1
 */
int UART_Read(JNIEnv *env, jclass clazz, jint port, jbyteArray data)
{
	int error = -1;
	int portIndex = -1;
	int fdIndex = -1;

	error = UART_SetPortConfig(env, port, &portIndex, &fdIndex);

	if(error >= 0)
	{
		if(mFd[fdIndex] != -1)
		{
			NativeUART *uartControl = NativeUART::getInstance();
			int bufferLength = env->GetArrayLength(data);
			jbyte* buffer = env->GetByteArrayElements(data,0);
			unsigned char* buffer_native = (unsigned char*)buffer;

//			ALOGD("Reading start");

			error = uartControl->UART_Read(mFd[fdIndex], bufferLength, buffer_native);

			env->ReleaseByteArrayElements(data, buffer,JNI_ABORT);

			// if error >= 0, it is numbers of bytes.
			if(error >= 0)
			{
//				ALOGD("Reading succeed %d", mFd[fdIndex]);
			}
			else
			{
				// fail to write
				UART_ThrowFailOperationException(env, "uart reading fails");
			}
		}
		else
		{
			// port is not opened
			UART_ThrowFailOperationException(env, "Port is not opened.");
		}
	}
	else
	{
		// error
		UART_ThrowFailOperationException(env, "Port configure setting is error.");
	}

	return error;
}

//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] = {
    {"openUART",     "(I)V", (void*) UART_Open },
    {"closeUART",     "(I)V", (void*) UART_Close },
    {"writeUART",     "(I[B)V", (void*) UART_Write },
    {"readUART",     "(I[B)I", (void*) UART_Read },

};


/**
 * Register JNINativeMethod function table to DVM.
 * 
 * return int [out] If registering succeeds, return 0. Otherwise, return -1.
 * Range:
 * 0: succeed to register UART
 * -1: fail to register UART
 * Unit: int
 * Scaling: 1
 * 
 * @param env    [in] JNIEnv is jni environment pointer. JNIEnv object is provided
 * by Android SDK.
 */
int register_custinterface_customizedhwmanager_uart(JNIEnv* env)
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
