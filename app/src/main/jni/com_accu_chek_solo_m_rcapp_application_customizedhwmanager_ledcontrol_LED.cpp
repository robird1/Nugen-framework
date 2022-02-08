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
#define LOG_TAG "LED_jni"

#include <jni.h>
#include <custHal.h>
#include <NativeLED.h>

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

typedef enum LED_ee_ledType
{
	INSULIN = 0x000F,
	STRIP = 0x0033,
	INFORMATION = 0x003C,
}LED_et_LedType;

//This is java full name qualifier, use to assign java servcie framework
static const char* const kClassName = "com/accu_chek/solo_m/rcapp/application/customizedhwmanager/ledcontrol/LED";


/**
 * Throw Fail Operation Exception to JAVA layer.
 * 
 * 
 * return None
 * throw OperationFailException if UART operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * provided by Android SDK.
 * @param message    [in] const char format of a string message in the exception.
 * char string constraints:
 * Range: Valid const char pointer
 * Unit: const char pointer
 * Scaling: 1
 */
int LED_ThrowFailOperationException(JNIEnv* env, const char* message)
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
 * Given LED type, the function turns the LED on.
 * 
 * 
 * return None
 * throw OperationFailException if opening LED operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv is
 * provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java service framework. jclass
 * provided by Android SDK.
 * @param led    [in] LED index in LED_ee_ledType
 * Range: (in LED_ee_ledType)
 * INSULIN = 0x000F,
 * STRIP = 0x0033,
 * INFORMATION = 0x003C,
 * Unit: jint
 * Scaling: 1
 */
void LED_Open(JNIEnv* env, jclass clazz, jint led)
{
	int result = -1;
	NativeLED *native = NativeLED::getInstance();

	result = native->LED_On(led);

	if(result < 0)
	{
		LED_ThrowFailOperationException(env, "Fail to open LED");
	}
}


/**
 * Given LED type, the function closes LED.
 * 
 * 
 * return None
 * throw OperationFailException if closing LED operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv is
 * provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * provided by Android SDK.
 * @param led    [in] LED index in LED_ee_ledType
 * Range: (in LED_ee_ledType)
 * INSULIN = 0x000F,
 * STRIP = 0x0033,
 * INFORMATION = 0x003C,
 * Unit: jint
 * Scaling: 1
 */
void LED_Close(JNIEnv* env, jclass clazz, jint led)
{
	int result = -1;
	NativeLED *native = NativeLED::getInstance();

	result = native->LED_Off(led);

	if(result < 0)
	{
		LED_ThrowFailOperationException(env, "Fail to close LED");
	}
}


/**
 * Given LED type, the function flashes LED.
 * 
 * 
 * return None
 * throw OperationFailException if closing LED operation fails.
 * throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv is
 * provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * provided by Android SDK.
 * @param frequency [in] LED frequency.
 * Range: Setting in SOLO M EMWR Specification
 * Unit: jint
 * Scaling: 1
 * @param led    [in] LED index in LED_ee_ledType
 * Range: (in LED_ee_ledType)
 * INSULIN = 0x000F,
 * STRIP = 0x0033,
 * INFORMATION = 0x003C,
 * Unit: jint
 * Scaling: 1
 */
void LED_Flash(JNIEnv* env, jclass clazz, jint frequency, jint led)
{
	int result = -1;
	NativeLED *native = NativeLED::getInstance();

	result = native->LED_Flash(frequency, led);

	if(result < 0)
	{
		LED_ThrowFailOperationException(env, "Fail to flash LED");
	}
}

//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] = {
    {"openLED",     "(I)V", (void*) LED_Open },
    {"closeLED",     "(I)V", (void*) LED_Close },
    {"flashLED",     "(II)V", (void*) LED_Flash },
};

/**
 * Register JNINativeMethod function table to DVM.
 * 
 * 
 * return int If registering succeeds, return 0. Otherwise, return -1.
 * Range:
 * 0: succeed to register LED
 * -1: fail to register LED
 * Unit: int
 * Scaling: 1
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv is
 * provided by Android SDK.
 */
int register_custinterface_customizedhwmanager_led(JNIEnv* env)
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
