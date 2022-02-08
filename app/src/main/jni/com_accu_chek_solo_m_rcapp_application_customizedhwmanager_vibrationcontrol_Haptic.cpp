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
#define LOG_TAG "HAPTIC_jni"

#include <jni.h>
#include <custHal.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <android/log.h>
#include <NativeHaptic.h>

// Log functions
#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )

//This is java full name qualifier, use to assign java service framework
static const char* const kClassName = "com/accu_chek/solo_m/rcapp/application/customizedhwmanager/vibrationcontrol/Haptic";

/**
 * An enumeration that is vibration index in Haptic JNI.
 */
typedef enum HAPTIC_ee_index
{
	TOUCH_FEEDBACK = 0x000F,
	THREE_FORTHS_SECOND = 0x0033,
	ONE_SECOND = 0x003C
}HAPTIC_et_Index;

/**
 * An enumeration that is vibration index in Haptic driver.
 */
typedef enum HAPTIC_ee_nativeType
{
	TYPE_TOUCH_FEEDBACK = 0x03,
	TYPE_Native_THREE_FORTHS_SECOND = 0x0f,
	TYPE_Native_ONE_SECOND = 0x10
}HAPTIC_et_NativeType;

/**
 * Throw Fail Operation Exception to JAVA layer.
 * 
 * 
 * return None
 * throw Throw OperationFailException if UART operation fails.
 * throw Throw ClassNotFoundException if exception class is not found.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param message    [in] const char format of a string message in the exception.
 * char string constraints:
 * Range: Valid const char pointer
 * Unit: const char pointer
 * Scaling: 1
 */
static int HAPTIC_ThrowFailOperationException(JNIEnv *env, const char *message)
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
 * Get driver's vibration index.
 * 
 * 
 * return 0 if javaIndex is legal. Otherwise, return -1.
 * Range:
 * 0: javaIndex is correct
 * -1: javaIndex is incorrect
 * Unit: N/A
 * Scaling: 1
 * 
 * @param javaIndex    [in] the integer index that input from upper layer in
 * HAPTIC_ee_index.
 * Range: (HAPTIC_ee_index)
 * TOUCH_FEEDBACK = 0x000F,
 * THREE_FORTHS_SECOND = 0x0033,
 * ONE_SECOND = 0x003C
 * Unit: N/A
 * Scaling: 1
 * @param index    [out] unsigned char pointer that store driver's vibration index
 * in HAPTIC_ee_nativeType.
 * Range: (HAPTIC_ee_nativeType)
 * TYPE_TOUCH_FEEDBACK = 0x03,
 * TYPE_Native_THREE_FORTHS_SECOND = 0x0f,
 * TYPE_Native_ONE_SECOND = 0x10
 * Unit: unsigned char pointer
 * Scaling: 1
 */
int HAPTIC_GetNativeIndex(int javaIndex, unsigned char *index)
{
	int error = 0;

	switch(javaIndex)
	{
	case TOUCH_FEEDBACK:
		*index = TYPE_TOUCH_FEEDBACK;
		break;
	case THREE_FORTHS_SECOND:
		*index = TYPE_Native_THREE_FORTHS_SECOND;
		break;
	case ONE_SECOND:
		*index = TYPE_Native_ONE_SECOND;
		break;
	default:
		error = -1;
		break;
	}

	return error;
}

/**
 * The function closes Haptic.
 * 
 * 
 * return None
 * throw Throw OprationFailException when closing Haptic fails.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv is
 * provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java service framework. jclass
 * is provided by Android SDK.
 */
void HAPTIC_Stop(JNIEnv *env, jobject clazz)
{
	int error = -1;
	NativeHaptic *hapticControl = NativeHaptic::getInstance();

	error = hapticControl->Haptic_Stop();

	if(error < 0)
	{
		HAPTIC_ThrowFailOperationException(env, "Fail to close haptic.");
	}
}


/**
 * Given haptic type, the function plays the style.
 * 
 * 
 * return void [out]
 * throw OprationFailException when playing Haptic fails.
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * object is provided by Android SDK.
 * @param thiz    [in] jclass is the object of the java service framework. jclass
 * object provided by Android SDK.
 * @param index    [in] haptic index in HAPTIC_ee_index
 * Range: (HAPTIC_ee_index)
 * TOUCH_FEEDBACK = 0x000F,
 * THREE_FORTHS_SECOND = 0x0033,
 * ONE_SECOND = 0x003C
 * Unit: jint
 * Scaling: 1
 */
void HAPTIC_Play(JNIEnv *env, jobject thiz, jint index)
{
	int error = -1;
	unsigned char charIndex;

	error = HAPTIC_GetNativeIndex(index, &charIndex);

	ALOGD("index=%d charindex=%d",index, charIndex);

	if(error >= 0)
	{
		NativeHaptic *hapticControl = NativeHaptic::getInstance();

		hapticControl->Haptic_Play(&charIndex);
	}
	else
	{
		HAPTIC_ThrowFailOperationException(env, "Error vibration index");
	}
}


//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] = {
    {"stopHaptic",     "()V", (void*) HAPTIC_Stop },
    {"playHaptic",     "(I)V", (void*) HAPTIC_Play },

};


/**
 * Register JNINativeMethod function table to DVM.
 * 
 * 
 * return int [out] If registering succeeds, return 0. Otherwise, return -1.
 * Range:
 * 0: succeed to register haptic
 * -1: fail to register haptic
 * Unit: int
 * Scaling: 1
 * 
 * @param env    [in] JNIEnv is jni environment pointer. JNIEnv object is provided
 * by Android SDK.
 */
int register_custinterface_customizedhwmanager_haptic(JNIEnv* env)
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
