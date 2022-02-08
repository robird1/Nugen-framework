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
#define LOG_TAG "ADC_jni"

#include <jni.h>
#include <custHal.h>

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

typedef enum ADC_ee_adcElement
{
	VCCM_FB = 0x000F,
	VCCN_3V_FB = 0x0033,
	VPROC_SNS = 0x003C,
	VSYS_SNS = 0x0055,
	NTC2 =  0x005A
} ADC_et_ADCElement;

typedef enum ADC_ee_adcElementChannel
{
	NATIVE_VPROC_SNS = 0,
	NATIVE_VCCN_3V_FB = 1,
	NATIVE_VCCM_FB = 2,
	NATIVE_VSYS_SNS = 3,
	NATIVE_NTC2 = 4
} ADC_et_ADCNativeChannel;

//This is java full name qualifier, use to assign java servcie framework
static const char* const kClassName = "com/accu_chek/solo_m/rcapp/application/customizedhwmanager/adcreader/ADC";


/**
 * Throw Fail Operation Exception to JAVA layer.
 * 
 * 
 * return None
 * throw OperationFailException if ADC reader operation fails.
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
int ADC_ThrowFailOperationException(JNIEnv* env, const char* message)
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
 * Get element channel.
 *
 * return None
 * throw OperationFailException if ADC reader operation fails.
 * throw ClassNotFoundException if exception class is not found.
 *
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * provided by Android SDK.
 * @param element    [in] the integer number of element index.
 * Range: in ADC_ee_adcElement
 * Unit: int
 * Scaling: 1
 */
int ADC_GetNativeIndex(JNIEnv* env, int element)
{
	int index = -1;

	switch(element)
	{
	case VCCM_FB:
		index = NATIVE_VCCM_FB;

		break;
	case VCCN_3V_FB:
		index = NATIVE_VCCN_3V_FB;

		break;
	case VPROC_SNS:
		index = NATIVE_VPROC_SNS;

		break;
	case VSYS_SNS:
		index = NATIVE_VSYS_SNS;

		break;
	 case NTC2:
	    index = NATIVE_NTC2;

	        break;
	default:
		ADC_ThrowFailOperationException(env, "The element does not exist.");
	}

	return index;
}

/**
 * Get the voltage of the element.
 *
 * return float if succeed to get the voltage of the element.
 * throw OperationFailException if ADC reader operation fails.
 * throw ClassNotFoundException if exception class is not found.
 *
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java service framework. jclass
 * provided by Android SDK.
 * @param element    [in] the integer number of element index.
 * Range: in ADC_ee_adcElement
 * Unit: int
 * Scaling: 1
 */
float ADC_GetVoltageValue(JNIEnv* env, jclass clazz, jint element)
{
	int result = -1;
	float value = -1;
	CUSTHAL *halOperation = CUSTHAL::getInstance();
	int native_index = ADC_GetNativeIndex(env, element);

	result = halOperation->getADCValue(native_index, &value);

	ALOGE("native index= %d, value = %f", native_index, value);

	if(result < 0)
	{
		ADC_ThrowFailOperationException(env, "Fail to get ADC voltage");
	}

	return value;
}

/**
 * Get all interrupt voltage values.
 *
 * return None
 * throw OperationFailException if ADC reader operation fails.
 * throw ClassNotFoundException if exception class is not found.
 *
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv
 * provided by Android SDK.
 * @param clazz    [in] jclass is the object of the java service framework. jclass
 * provided by Android SDK.
 * @param data    [out] a jfloatArray that is to store interrupt voltage values.
 * Range: 4
 * Unit: float
 * Scaling: 1
 */
void ADC_GetInterruptEventData(JNIEnv* env, jclass clazz, jfloatArray data)
{
	int result = -1;
	jfloat* data_ref = env->GetFloatArrayElements(data,0);
	CUSTHAL *halOperation = CUSTHAL::getInstance();
	ADC_INT_RESULT adc_int_result;

	result = halOperation->getInterruptEventData(&adc_int_result);

	data_ref[0] = adc_int_result.fChannelValue[0];
	data_ref[1] = adc_int_result.fChannelValue[1];
	data_ref[2] = adc_int_result.fChannelValue[2];
	data_ref[3] = adc_int_result.fChannelValue[3];

	env->ReleaseFloatArrayElements(data, data_ref, JNI_ABORT);

	if(result < 0)
	{
		ADC_ThrowFailOperationException(env, "Fail to get ADC interrupt element data");
	}
}


//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] = {
    {"getADCVoltageValue",     "(I)F", (void*) ADC_GetVoltageValue },
    {"getADCInterruptEventData",     "([F)V", (void*) ADC_GetInterruptEventData },
};

/**
 * Register JNINativeMethod function table to DVM.
 * 
 * 
 * return int If registering succeeds, return 0. Otherwise, return -1.
 * Range:
 * 0: succeed to register ADC
 * -1: fail to register ADC
 * Unit: int
 * Scaling: 1
 * 
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv is
 * provided by Android SDK.
 */
int register_custinterface_customizedhwmanager_adc(JNIEnv* env)
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
