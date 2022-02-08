///////////////////////////////////////////////////////////
//  NativeLED.h
//  Implementation of the Class NativeLED
//  Created on:      24-2-2015 �W�� 11:27:48
//  Original author: ChristinaJiang
///////////////////////////////////////////////////////////

#ifndef LED_H
#define LED_H

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <android/log.h>

// Log functions
#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )

#define RETRY_TIME 10

/*global define the hardware path*/
#define THE_DEVICE_INSULIN2 "/sys/class/leds/insulin2/brightness"
#define THE_DEVICE_INSULIN3 "/sys/class/leds/insulin3/brightness"
#define THE_DEVICE_STRIP "/sys/class/leds/connector0/brightness"
#define THE_DEVICE_RED "/sys/class/leds/red/brightness"
#define THE_DEVICE_RED_TRIGGER "/sys/class/leds/red/trigger"
#define THE_DEVICE_RED_DELAY_OFF "/sys/class/leds/red/delay_off"
#define THE_DEVICE_RED_DELAY_ON "/sys/class/leds/red/delay_on"

/*global define the led blinking timer(ms)*/
/*
#define LED_FLASH01_Hz 5000
#define LED_FLASH02_Hz 2500
#define LED_FLASH05_Hz 1000
#define LED_FLASH1_Hz 500
#define LED_FLASH2_Hz 250
#define LED_FLASH_200Hz 200
*/
/*Led active mode
 * LED_FLASH02,
    LED_FLASH05,
    LED_FLASH1,
    LED_FLASH2,
    LED_FLASH5
 *
 * */

/**
 * LED types
 */
enum LED_ee_nativeLEDType
{
	/**
	 * The LED is lighted for reminding actions about insulin.
	 */
    INSULIN_LED = 0x000F,

    /**
	 * The LED is at the entrance which inserts a stripe.
	 */
    STRIP_LED = 0x0033,

    /**
	 * The LED is for reminding phone's user to watch information.
	 */
    INFORMATION_LED = 0x003C,
};

/**
 * LED switch modes
 */
enum NATIVE_LED_Switch
{
	/**
	 * close LED
	 */
	CLOSE = 0,

	/**
	 * open LED
	 */
	OPEN = 1
};

class NativeLED
{

public:
	/**
	 * NativeLED Destructor that does nothing here.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
	virtual ~NativeLED();

	/**
	 * Get NativeLED instance for implementing Singleton pattern.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
	static NativeLED* getInstance();

	/**
	 * Given LED type, the function flashes LED. The frequency is 200mHz.
	 * 
	 * @param mode [in] LED type in LED_ee_nativeLEDType
	 * Range: (in LED_ee_nativeLEDType)
	 * BLUE_LED: 0x003C
	 * Unit: N/A
	 * Scaling: 1
	 * @return int [out] Return -1 if fail to flash LED. Otherwise, return 0.
	 * Range:
	 * -1: fail to flash LED
	 * 0: succeed to flash LED
	 * Unit: N/A
	 * Scaling: 1
	 */
	int LED_Flash(int frequency, int mode);
	/**
	 * Given LED type, the function closes LED.
	 * 
	 * @param mode [in] LED type in LED_ee_nativeLEDType
	 * Range: (in LED_ee_nativeLEDType)
	 * INSULIN_LED: 0x000F
	 * STRIP_LED: 0x0033
	 * BLUE_LED: 0x003C
	 * Unit: N/A
	 * Scaling: 1
	 * @return int [out] Return -1 if fail to close LED. Otherwise, return 0.
	 * Range:
	 * -1: fail to close LED
	 * 0: succeed to close LED
	 * Unit: N/A
	 * Scaling: 1
	 */
	int LED_Off(int mode);
	/**
	 * Given LED type, the function turns the LED on.
	 * 
	 * @param mode [in] LED type in LED_ee_nativeLEDType
	 * Range: (in LED_ee_nativeLEDType)
	 * INSULIN_LED: 0x000F
	 * STRIP_LED: 0x0033
	 * BLUE_LED: 0x003C
	 * Unit: N/A
	 * Scaling: 1
	 * @return int [out] Return -1 if fail to open LED. Otherwise, return 0.
	 * Range:
	 * -1: fail to open LED
	 * 0: succeed to open LED
	 * Unit: N/A
	 * Scaling: 1
	 */
	int LED_On(int mode);

private:
	/**
	 * NativeLED Constructor does nothing here, but protecting the constructor
	 * meets Sigleton pattern.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
	NativeLED();

	/**
	 * NativeLED copy does nothing here, but protecting the function
	 * meets Sigleton pattern.
	 *
	 * @param NativeLED&    [in] copy another NativeLED object.
	 * @return None [out]
	 */
//	NativeLED(const NativeLED&);

	/**
	 * Given the path of LED device and whether the LED is enable or not,
	 * the function writes the action into the device's file and will drive the device work.
	 *
	 * @param path [in] the string path of LED device
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
	 * @param command [in] an string value that makes LED device work.
	 * 0 means closing the light. 1 means opening the light.
	 * Other positive integer value works (flash frequency) when LED flash is initialized.
	 * "timer" command can initialize LED flash function.
	 * Range:
	 * "0": close the light
	 * "1": open the light
	 * positive integer value: flash frequency
	 * "timer": initialize LED flash function
	 * Unit: N/A
	 * Scaling: 1
	 * @param size [in] command string length
	 * Range: 0 ... max interger number
	 * Unit: char
	 * Scaling: 1
	 * @return int [out] Return -1 if fail to control LED. Otherwise, return 0.
	 * Range:
	 * 0: succeed to control LED
	 * -1: fail to control LED
	 * Unit: N/A
	 * Scaling: 1
	 */
	int LED_Control(const char* path, char* command, int size);

	/**
	 * Given the path of LED device and whether the LED is enable or not,
	 * the function writes the action into the device's file and will drive the led on/ off.
	 *
	 * @param path [in] the string path of LED device
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
	 * @param enable [in] an string value that makes LED device work.
	 * 0 means closing the light. 1 means opening the light.
	 * Other positive integer value works (flash frequency) when LED flash is initialized.
	 * Range:
	 * "0": close the light
	 * "1": open the light
	 * positive integer value: flash frequency
	 * Unit: N/A
	 * Scaling: 1
	 * @return int [out] Return -1 if fail to control LED. Otherwise, return 0.
	 * Range:
	 * 0: succeed to control LED
	 * -1: fail to control LED
	 * Unit: N/A
	 * Scaling: 1
	 */
	int LED_Control(const char* path, int* enable);

	/**
	 * Given LED type, the function turns LEDs on/off.
	 * 
	 * @param mode [in] LED type in LED_ee_nativeLEDType.
	 * Range: (in LED_ee_nativeLEDType)
	 * INSULIN_LED: 0x000F
	 * STRIP_LED: 0x0033
	 * BLUE_LED: 0x003C
	 * Unit: N/A
	 * Scaling: 1
	 * @param enable [in] an integer value that makes LED device work. 0 means closing the light. 1 means opening the light.
	 * Range:
	 * 0: close LED
	 * 1: open LED
	 * Unit: N/A
	 * Scaling: 1
	 * @return int [out] Return -1 if fail to switch LED. Otherwise, return 0.
	 * Range:
	 * 0: succeed to switch LED
	 * -1: fail to switch LED
	 * Unit: N/A
	 * Scaling: 1
	 */
	int LED_SwitchLight(int mode, int enable);

	/**
	 * Given LED type, the function initializes LED flash setting.
	 *
	 * @param mode [in] LED type in LED_ee_nativeLEDType, but the function is only for BLUE LED.
	 * Range: (in LED_ee_nativeLEDType)
	 * BLUE_LED: 0x003C
	 * Unit: N/A
	 * Scaling: 1
	 * @param deviceDelayOffPath [out] store the path of device delay off
	 * Range:
	 * THE_DEVICE_RED_DELAY_OFF
	 * Unit: N/A
	 * Scaling: 1
	 * @param deviceDelayOnPath [out] store the path of device delay on
	 * Range:
	 * THE_DEVICE_RED_DELAY_ON
	 * Unit: N/A
	 * Scaling: 1
	 * @return int [out] Return -1 if fail to initialize LED flash. Otherwise, return 0.
	 * Range:
	 * 0: succeed to initialize LED flash
	 * -1: fail to initialize LED flash
	 * Unit: N/A
	 * Scaling: 1
	 */
	int LED_InitFlash(int mode, const char ** deviceDelayOffPath, const char ** deviceDelayOnPath);

};
#endif // LED_H
