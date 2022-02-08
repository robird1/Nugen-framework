#include "NativeLED.h"

#define LOG_NDEBUG 0
#define LOG_TAG "LED"

/**
 * NativeLED Constructor does nothing here, but protecting the constructor
 * meets Sigleton pattern.
 * 
 * @param None    [in]
 * @return None [out]
 */
	NativeLED::NativeLED()
    {
    	ALOGI("init uart ");
    }

/**
 * NativeLED Destructor that does nothing here.
 * 
 * @param None    [in]
 * @return None [out]
 */
	NativeLED::~NativeLED()
    {
    	ALOGI("finish uart ");
    }

/**
 * Get NativeLED instance for implementing Singleton pattern.
 * 
 * @param None    [in]
 * @return None [out]
 */
    NativeLED* NativeLED::getInstance()
	{
		static NativeLED *pInstance = new NativeLED;

		return pInstance;
	}

/**
 * Given LED type, the function closes LED.
 * 
 * @return int [out] Return -1 if fail to close LED. Otherwise, return 0.
 * Range:
 * -1: fail to close LED
 * 0: succeed to close LED
 * Unit: N/A
 * Scaling: 1
 * 
 * @param mode    [in] LED type in LED_ee_nativeLEDType
 * Range: (in LED_ee_nativeLEDType)
 * INSULIN_LED: 0x000F
 * STRIP_LED: 0x0033
 * INFORMATION_LED: 0x003C
 * Unit: N/A
 * Scaling: 1
 */
int NativeLED::LED_Off(int mode)
{

	ALOGV("[%s]: enter", __FUNCTION__);

	return LED_SwitchLight(mode, CLOSE);
}


/**
 * Given LED type, the function turns the LED on.
 * 
 * @return int [out] Return -1 if fail to open LED. Otherwise, return 0.
 * Range:
 * -1: fail to open LED
 * 0: succeed to open LED
 * Unit: N/A
 * Scaling: 1
 * 
 * @param mode    [in] LED type in LED_ee_nativeLEDType
 * Range: (in LED_ee_nativeLEDType)
 * INSULIN_LED: 0x000F
 * STRIP_LED: 0x0033
 * INFORMATION_LED: 0x003C
 * Unit: N/A
 * Scaling: 1
 */
int NativeLED::LED_On(int mode)
{
	 ALOGV("[%s]: enter", __FUNCTION__);

	 /* constant on, up to maximum allowed time */
	 return LED_SwitchLight(mode, OPEN);
}


/**
 * Given LED type, the function flashes LED.
 * 
 * @return int [out] Return -1 if fail to flash LED. Otherwise, return 0.
 * Range:
 * -1: fail to flash LED
 * 0: succeed to flash LED
 * Unit: N/A
 * Scaling: 1
 * 
 * @param mode    [in] LED type in LED_ee_nativeLEDType
 * Range: (in LED_ee_nativeLEDType)
 * INFORMATION_LED: 0x003C
 * Unit: N/A
 * Scaling: 1
 */
int NativeLED::LED_Flash(int frequency, int mode)
{
	ALOGV("[%s]: enter", __FUNCTION__);

		int i = 0;
		int controlResult = -1;
		int result = -1;
	    char value[20];
	    const char *deviceDelayOffPath;
	    const char *deviceDelayOnPath;

	    ALOGV("[%s]: red flash led on", __FUNCTION__);
	    controlResult = LED_InitFlash(mode, &deviceDelayOffPath, &deviceDelayOnPath);

		if(-1 != controlResult)
		{
			int retryCount = 0;
			int delayOffAccess = -1;
			int delayOnAccess = -1;
			bool allowRetry = true;
			int hz = frequency;

			//after initial, wait for dalay off path and delay on path
			do
			{
				delayOffAccess = access(deviceDelayOffPath, F_OK);
				delayOnAccess = access(deviceDelayOnPath, R_OK|W_OK);

				retryCount++;

				allowRetry = ((delayOffAccess < 0) || (delayOnAccess < 0)) && (retryCount < RETRY_TIME);

			}while(allowRetry);

			controlResult = LED_Control(deviceDelayOffPath, &hz);
			result = LED_Control(deviceDelayOnPath, &hz);
			controlResult += result;
		}

	    return controlResult;
}


/*
 * hide function
 */

/**
 * Given the path of LED device and whether the LED is enable or not,
 * the function writes the action into the device's file and will drive the led
 * on/ off.
 * 
 * @return int [out] Return -1 if fail to control LED. Otherwise, return 0.
 * Range:
 * 0: succeed to control LED
 * -1: fail to control LED
 * Unit: N/A
 * Scaling: 1
 * 
 * @param path    [in] the string path of LED device
 * Range:
 * THE_DEVICE_INSULIN2
 * THE_DEVICE_INSULIN3
 * THE_DEVICE_STRIP
 * THE_DEVICE_RED
 * THE_DEVICE_RED_TRIGGER
 * THE_DEVICE_RED_DELAY_OFF
 * THE_DEVICE_RED_DELAY_ON
 * Unit: N/A
 * Scaling: 1
 * @param enable    [in] an string value that makes LED device work.
 * 0 means closing the light. 1 means opening the light.
 * Other positive integer value works (flash frequency) when LED flash is
 * initialized.
 * Range:
 * "0": close the light
 * "1": open the light
 * positive integer value: flash frequency
 * Unit: N/A
 * Scaling: 1
 */
int NativeLED::LED_Control(const char* path, int* enable)
{
	ALOGV("[%s]: enter", __FUNCTION__);
	char value[20];
	int printLetterNumber = -1;
	int result = -1;

	printLetterNumber = sprintf(value, "%d\n", *enable);
	result = LED_Control(path, value, printLetterNumber);

	return result;
}

/**
 * Given the path of LED device and whether the LED is enable or not,
 * the function writes the action into the device's file and will drive the device
 * work.
 * 
 * @return int [out] Return -1 if fail to control LED. Otherwise, return 0.
 * Range:
 * 0: succeed to control LED
 * -1: fail to control LED
 * Unit: N/A
 * Scaling: 1
 * 
 * @param path    [in] the string path of LED device
 * Range:
 * THE_DEVICE_INSULIN2
 * THE_DEVICE_INSULIN3
 * THE_DEVICE_STRIP
 * THE_DEVICE_RED
 * THE_DEVICE_RED_TRIGGER
 * THE_DEVICE_RED_DELAY_OFF
 * THE_DEVICE_RED_DELAY_ON
 * Unit: N/A
 * Scaling: 1
 * @param command    [in] an string value that makes LED device work.
 * 0 means closing the light. 1 means opening the light.
 * Other positive integer value works (flash frequency) when LED flash is
 * initialized.
 * "timer" command can initialize LED flash function.
 * Range:
 * "0": close the light
 * "1": open the light
 * positive integer value: flash frequency
 * "timer": initialize LED flash function
 * Unit: N/A
 * Scaling: 1
 * @param size    [in] command string length
 * Range: 0 ... max interger number
 * Unit: char
 * Scaling: 1
 */
int NativeLED::LED_Control(const char* path, char* command, int size)
{
	ALOGV("[%s]: enter", __FUNCTION__);
	int writtenLetterNumber = -1;
	int fd = -1;
	int result = -1;

	//open
	fd = open(path, O_RDWR);

	if(fd >= 0)
	{
		//write
		writtenLetterNumber = write(fd, command, size);

		//close
		close(fd);

		if(size == writtenLetterNumber)
		{
			result = 0;
		}
	}
	else
	{
		ALOGD("[%s]: errno = %d", __FUNCTION__, errno);
	}

	return result;
}


/**
 * Given LED type, the function turns LEDs on/off.
 * 
 * @return int [out] Return -1 if fail to switch LED. Otherwise, return 0.
 * Range:
 * 0: succeed to switch LED
 * -1: fail to switch LED
 * Unit: N/A
 * Scaling: 1
 * 
 * @param mode    [in] LED type in LED_ee_nativeLEDType.
 * Range: (in LED_ee_nativeLEDType)
 * INSULIN_LED: 0x000F
 * STRIP_LED: 0x0033
 * INFORMATION_LED: 0x003C
 * Unit: N/A
 * Scaling: 1
 * @param enable    [in] an integer value that makes LED device work. 0 means
 * closing the light. 1 means opening the light.
 * Range:
 * 0: close LED
 * 1: open LED
 * Unit: N/A
 * Scaling: 1
 */
int NativeLED::LED_SwitchLight(int mode, int enable)
{

    ALOGV("[%s]: enter", __FUNCTION__);
    int result = -1;
    int controlResult = 0;

    switch(mode)
    {
	case INSULIN_LED :
        ALOGV("[%s]: insulin led on/off", __FUNCTION__);
        controlResult = LED_Control(THE_DEVICE_INSULIN2, &enable);

        result = LED_Control(THE_DEVICE_INSULIN3, &enable);
        controlResult += result;
		break;

 	case STRIP_LED :
        ALOGV("[%s]: strip led on/off", __FUNCTION__);
        controlResult = LED_Control(THE_DEVICE_STRIP, &enable);
        break;

    case INFORMATION_LED :
		ALOGV("[%s]: red led on/off", __FUNCTION__);
		controlResult = LED_Control(THE_DEVICE_RED, &enable);
        break;

    default:
    	ALOGV("[%s]: wrong led type", __FUNCTION__);
    	controlResult = -1;
		break;

    }

    return controlResult;
}


/**
 * Given LED type, the function initializes LED flash setting.
 * 
 * @return int [out] Return -1 if fail to initialize LED flash. Otherwise, return
 * 0.
 * Range:
 * 0: succeed to initialize LED flash
 * -1: fail to initialize LED flash
 * Unit: N/A
 * Scaling: 1
 * 
 * @param mode    [in] LED type in LED_ee_nativeLEDType, but the function is only
 * for BLUE LED.
 * Range: (in LED_ee_nativeLEDType)
 * INFORMATION_LED: 0x003C
 * Unit: N/A
 * Scaling: 1
 * @param deviceDelayOffPath    [out] store the path of device delay off
 * Range:
 * THE_DEVICE_RED_DELAY_OFF
 * Unit: N/A
 * Scaling: 1
 * @param deviceDelayOnPath    [out] store the path of device delay on
 * Range:
 * THE_DEVICE_RED_DELAY_ON
 * Unit: N/A
 * Scaling: 1
 */
int NativeLED::LED_InitFlash(int mode, const char **offPath, const char **onPath)
{
    ALOGV("[%s]: enter", __FUNCTION__);


	int fd = -1;
    int result = -1;

    switch(mode)
    {
    case INFORMATION_LED :
    {
    	char command[] = "timer";
    	int size = strlen(command);

    	result = LED_Control(THE_DEVICE_RED_TRIGGER, command, size);

    	if(result == 0)
    	{
    		*offPath = THE_DEVICE_RED_DELAY_OFF;
    		*onPath = THE_DEVICE_RED_DELAY_ON;

    		ALOGV("[%s]: enter %s", __FUNCTION__, *offPath);
    	}

    break;
    }

    default:
    	result = -1;
    	break;
    }

    return result;
}
