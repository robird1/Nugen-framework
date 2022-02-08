///////////////////////////////////////////////////////////
//  NativeLED.h
//  Implementation of the Class NativeLED
//  Created on:      24-2-2015 �W�� 11:27:48
//  Original author: ChristinaJiang
///////////////////////////////////////////////////////////

#ifndef HAPTIC_H
#define HAPTIC_H

#include <jni.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <android/log.h>
#include <NativeI2CControl.h>

// Log functions
#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )

#define HAPTIC_SLAVE_BASE_ADDRESS 0x5a
#define HAPTIC_PATH "/dev/i2c-1"
#define REAL_TIME_PLAYBACK_STRENGTH 0x7F
#define RETRY_TIMES 2

/*
** haptic actuator type
*/
typedef enum HAPTIC_ee_actuatorType
{
	ERM,
	LRA
} HAPTIC_et_ActuatorType;

/*
** haptic loop type
*/
typedef enum HAPTIC_ee_loopType
{
	OPEN_LOOP,
	CLOSE_LOOP
} HAPTIC_et_LoopType;

/*
** actuator definition structure
*/
typedef struct Haptic_ss_Actuator {
	/*
	** device_type: ERM or LRA in HAPTIC_ee_actuatorType
	*/
	HAPTIC_et_ActuatorType device_type;
	/*
	** g_effect_bank: LIBRARY_B
	*/
	unsigned char	g_effect_bank;
	/*
	** loop: OPEN_LOOP or CLOSE_LOOP in HAPTIC_ee_loopType
	*/
	HAPTIC_et_LoopType	loop;
	/*
	** rated_vol: 0x3d
	*/
	unsigned char 	rated_vol;
	/*
	** over_drive_vol: 0x4d
	*/
	unsigned char 	over_drive_vol;
	/*
	** drive_time: 0x13
	*/
	unsigned char	drive_time;
} Haptic_st_Actuator;

/*
** Haptic register
*/
typedef enum HAPTIC_ee_register
{
	STATUS_REG  = 0x00,
	MODE_REG = 0x01,
	REAL_TIME_PLAYBACK_REG = 0x02,
	LIBRARY_SELECTION_REG = 0x03,
	WAVEFORM_SEQUENCER_REG = 0x04,
	GO_REG = 0x0C,
	/*
	** OverDrive Time Offset
	*/
	OVERDRIVE_TIME_OFFSET_REG = 0x0D,

	/*
	** Sustain Time Offset, postive
	*/
	SUSTAIN_TIME_OFFSET_POS_REG = 0x0E,

	/*
	** Sustain Time Offset, negative
	*/
	SUSTAIN_TIME_OFFSET_NEG_REG = 0x0F,

	/*
	** Brake Time Offset
	*/
	BRAKE_TIME_OFFSET_REG = 0x10,

	/*
	** Audio to Haptics Control
	*/
	AUDIO_HAPTICS_CONTROL_REG = 0x11,
	/*
	** Audio to Haptics Minimum Input Level
	*/
	AUDIO_HAPTICS_MIN_INPUT_REG = 0x12,

	/*
	** Audio to Haptics Maximum Input Level
	*/
	AUDIO_HAPTICS_MAX_INPUT_REG = 0x13,

	/*
	** Audio to Haptics Minimum Output Drive
	*/
	AUDIO_HAPTICS_MIN_OUTPUT_REG = 0x14,

	/*
	** Audio to Haptics Maximum Output Drive
	*/
	AUDIO_HAPTICS_MAX_OUTPUT_REG = 0x15,

	/*
	** Rated Voltage
	*/
	RATED_VOLTAGE_REG = 0x16,

	/*
	** Overdrive Clamp Voltage
	*/
	OVERDRIVE_CLAMP_VOLTAGE_REG = 0x17,

	/*
	** Auto Calibrationi Compensation Result
	*/
	AUTO_CALI_RESULT_REG = 0x18,

	/*
	** Auto Calibration Back-EMF Result
	*/
	AUTO_CALI_BACK_EMF_RESULT_REG = 0x19,

	/*
	** Feedback Control
	*/
	FEEDBACK_CONTROL_REG = 0x1A,

	/*
	** Control1
	*/
	Control1_REG = 0x1B,

	/*
	** Control2
	*/
	Control2_REG = 0x1C,

	/*
	** Control3
	*/
	Control3_REG = 0x1D,

	/*
	** Auto Calibration Memory Interface
	*/
	AUTOCAL_MEM_INTERFACE_REG = 0x1E,


} HAPTIC_et_Register;

/*
** Haptic register status content
*/
typedef enum HAPTIC_ee_statusSet
{
    STATUS_DEFAULT = 0x00,
    DIAG_RESULT_MASK = (1 << 3),
    AUTO_CAL_PASSED = (0 << 3),
    AUTO_CAL_FAILED = (1 << 3),
    DIAG_GOOD = (0 << 3),
    DIAG_BAD = (1 << 3)
} HAPTIC_et_StatusSet;

/*
** Haptic register mode content
*/
typedef enum HAPTIC_ee_modeSet
{
	MODE_STANDBY = 0x40,
	MODE_INTERNAL_TRIGGER = 0,
	MODE_DIAGNOSTICS = 6,
	AUTO_CALIBRATION = 7

} HAPTIC_et_ModeSet;

/*
** Haptic register waveform content
*/
typedef enum HAPTIC_ee_waveformSet
{
	WAVEFORM_SEQUENCER_REG_VALUE = 0x0f

} HAPTIC_et_WaveFormSet;

/*
** Haptic register go content
*/
typedef enum HAPTIC_ee_goSet
{
	GO = 0x01,
	STOP = 0x00
} HAPTIC_et_GoSet;

/*
** Haptic register feedback control content
*/
typedef enum HAPTIC_ee_feedbackControlSet
{
	FEEDBACK_CONTROL_DEVICE_TYPE_MASK = 0x80
} HAPTIC_et_FeedbackBackControlSet;

/*
** Haptic register Auto Calibration Memory Interface content
*/
typedef enum HAPTIC_ee_memInterfaceSet
{
	AUTOCAL_MEM_INTERFACE_REG_OTP_MASK = 0x04

} HAPTIC_et_MemInterfaceSet;

/*
** Haptic register control1 content
*/
typedef enum HAPTIC_ee_control1Set
{
	Control1_REG_DRIVE_TIME_MASK = 0x1f

} HAPTIC_et_Cotronl1Set;

/*
** Haptic register control3 content
*/
typedef enum HAPTIC_ee_control3Set
{
	Control3_REG_LOOP_MASK = 0x21,
	LRA_OpenLoop_Enabled = 0x01,
	ERM_OpenLoop_Enabled = (1 << 5)

} HAPTIC_et_Cotronl3Set;

/*
** haptic auto calibration register set flow
*/
static const unsigned char HAPTIC_calibrationSequence[] =
{
    MODE_REG,                       AUTO_CALIBRATION,
	REAL_TIME_PLAYBACK_REG,         REAL_TIME_PLAYBACK_STRENGTH,
    GO_REG,                         GO,
};

/*
** haptic diagnostic register set flow
*/
static const unsigned char HAPTIC_diagnosticSequence[] =
{
    MODE_REG,                       MODE_DIAGNOSTICS,
    GO_REG,                         GO,
};



class NativeHaptic
{

public:
	/**
	 * NativeHaptic Destructor that does nothing here.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
	virtual ~NativeHaptic();

	/**
	 * Get NativeHaptic instance for implementing Singleton pattern.
	 *
	 * @param None    [in]
	 * return None [out]
	 */
	static NativeHaptic* getInstance();

	/**
	 * Play specific haptic type, 0x0f.
	 *
	 * @param None    [in]
	 * @return int [out] Return negative integer number if there is any error. Otherwise, return 0.
	 * Range:
	 * 0: succeed to play default vibration.
	 * -1: fail to play default vibration.
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_PlayOnce();

	/**
	 * Stop haptic.
	 *
	 * @param None    [in]
	 * @return int [out] Return negative integer number if there is any error.
	 * Otherwise, return 0.
	 * Range:
	 * 0: succeed to close vibration.
	 * -1: fail to close vibration.
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_Stop();

	/**
	 * Given an haptic type, play it by the type.
	 *
	 * @param index [in] unsigned char pointer that is specific haptic type in the driver.
	 * The range is from 1 to 123.
	 * Range:
	 * 1 ... 123
	 * Unit: int
	 * Scaling: 1
	 * @return int [out] Return negative integer number if there is any error.
	 * Otherwise, return 0.
	 * Range:
	 * 0: succeed to play vibration.
	 * -1: fail to play vibration.
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_Play(unsigned char *index);

private:
	/**
	 * NativeHaptic Constructor does nothing here, but protecting the constructor
	 * meets Sigleton pattern.
	 *
	 * If OTP is 0, reset registers (RATED_VOLTAGE_REG/ OVERDRIVE_CLAMP_VOLTAGE_REG/ FEEDBACK_CONTROL_REG/ Control1_REG/ Control3_REG) according to Haptic_st_Actuator variable.
	 * If OTP is not 0, just reset control registers (Control1_REG and Control3_REG).
	 *
	 * Finally, set MODE_REG to MODE_STANDBY.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
	NativeHaptic();

	/**
	 * Open Haptic device and return its file descriptor.
	 *
	 * @param path    [in] String of the Haptic device path.
	 * Range:
	 * HAPTIC_PATH: "/dev/i2c-1"
	 * Unit: const char pointer
	 * Scaling: 1
	 * @param addr [in] Haptic device's address
	 * Range:
	 * HAPTIC_SLAVE_BASE_ADDRESS: 0x5a.
	 * Unit: unsigned int
	 * Scaling: 1
	 * @return int [out] Return negative integer number if there is any error.
	 * Otherwise, return file descriptor.
	 * Range:
	 * 0: succeed to open vibration device.
	 * -1: fail to open vibration device.
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_OpenDevice(const char* path, unsigned int addr);

	/**
	 * Calibrate Haptic according calibration sequence.
	 * The sequence is
	 * 1. Set MODE_REG to AUTO_CALIBRATION.
	 * 2. Set REAL_TIME_PLAYBACK_REG to REAL_TIME_PLAYBACK_STRENGTH.
	 * 3. Set GO_REG to GO.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @return int [out] Return negative integer number if there is any error.
	 * Otherwise, return 0.
	 * Range:
	 * -1: fail to haptic calibration
	 * 0: succeed to haptic calibration
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_AutoCalibration(int fd);

	/**
	 * Diagnose Haptic according diagnostic Sequence.
	 * The sequence is
	 * 1. Set MODE_REG to MODE_DIAGNOSTICS.
	 * 2. Set GO_REG to GO.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @return int [out] Return negative integer number if there is any error.
	 * Otherwise, return 0.
	 * Range:
	 * -1: fail to haptic diagnostic
	 * 0: succeed to haptic diagnostic
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_Diagnostic(int fd);

	/**
	 * Set registers, RATED_VOLTAGE_REG/ OVERDRIVE_CLAMP_VOLTAGE_REG/ FEEDBACK_CONTROL_REG.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @param setting [in] Haptic_st_Actuator variables that includes registers' values.
	 * Range:
	 * device_type: ERM or LRA in HAPTIC_ee_actuatorType
	 * g_effect_bank: LIBRARY_B
	 * loop: OPEN_LOOP or CLOSE_LOOP in HAPTIC_ee_loopType
	 * rated_vol: 0x3d
	 * over_drive_vol: 0x4d
	 * drive_time: 0x13
	 * Unit: Haptic_st_Actuator
	 * Scaling: 1
	 * @return None [out]
	 */
	void Haptic_ResetRegBySetting(int fd, Haptic_st_Actuator setting);

	/**
	 * Set registers, Control1_REG and Control3_REG.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @param setting [in] Haptic_st_Actuator variables that includes registers' values.
	 * Range:
	 * device_type: ERM or LRA in HAPTIC_ee_actuatorType
	 * g_effect_bank: LIBRARY_B
	 * loop: OPEN_LOOP or CLOSE_LOOP in HAPTIC_ee_loopType
	 * rated_vol: 0x3d
	 * over_drive_vol: 0x4d
	 * drive_time: 0x13
	 * Unit: Haptic_st_Actuator
	 * Scaling: 1
	 * @return None [out]
	 */
	void Haptic_SetControlBySetting(int fd, Haptic_st_Actuator setting);

	/**
	 * Given register and value, the function writes the value into the register.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @param reg [in] register index
	 * Range:
	 * The register name is listed in HAPTIC_ee_register.
	 * Unit: unsigned char
	 * Scaling: 1
	 * @param value [in] the value that is filled into the register
	 * Range:
	 * The register value is listed in HAPTIC_ee_statusSet (Status register), HAPTIC_ee_modeSet (Mode register), HAPTIC_ee_waveformSet (Waveform register), HAPTIC_ee_goSet (Go register) and HAPTIC_ee_control3Set (Control3 register).
	 * Unit: unsigned char
	 * Scaling: 1
	 * @return int [out] Return negative integer number if there is any error.
	 * Otherwise, return 0.
	 * Range:
	 * 0: succeed to write value to register
	 * -1: fail to write value to register
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_WriteRegValue(int fd, unsigned char reg, unsigned char value);

	/**
	 * Given the array of registers and values, the function writes each value into each register.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @param data [in] unsigned char pointer that points an array.
	 * The format of the array is {register_name, register_value, ....}.
	 * register_name Range:
	 * The register name is listed in HAPTIC_ee_register.
	 * Unit: const unsigned char pointer
	 * Scaling: 1
	 * register_value Range:
	 * The register value is listed in HAPTIC_ee_statusSet (Status register), HAPTIC_ee_modeSet (Mode register), HAPTIC_ee_waveformSet (Waveform register), HAPTIC_ee_goSet (Go register) and HAPTIC_ee_control3Set (Control3 register).
	 * Unit: N/A
	 * Scaling: 1
	 * @param size [in] the size of data
	 * Range:
	 * 2 ... max unsigned int that is multiplies of 2
	 * Unit: unsigned int
	 * Scaling: 1
	 * @return int [out] Return negative integer number if there is any error.
	 * Otherwise, return 0.
	 * Range:
	 * 0: succeed to write value to register
	 * -1: fail to write value to register
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_WriteRegValue(int fd, const unsigned char* data,	unsigned int size);

	/**
	 * Given register, mask and value, the function writes the value into the position after masking the register .
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @param reg [in] register index
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: unsigned char
	 * Scaling: 1
	 * @param reg [in] register index
	 * Range:
	 * The register name is listed in HAPTIC_ee_register.
	 * Unit: N/A
	 * Scaling: 1
	 * @param mask [in] the position that is filled in the register
	 * Range:
	 * HAPTIC_ee_statusSet (status register)
	 * DIAG_RESULT_MASK = (1 << 3),
	 * HAPTIC_ee_feedbackControlSet (feedboack control register)
	 * FEEDBACK_CONTROL_DEVICE_TYPE_MASK = 0x80
	 * HAPTIC_ee_memInterfaceSet (Haptic Auto Calibration Memory Interface register)
	 * AUTOCAL_MEM_INTERFACE_REG_OTP_MASK = 0x04
	 * HAPTIC_ee_control1Set (Control 1 register)
	 * Control1_REG_DRIVE_TIME_MASK = 0x1f
	 * HAPTIC_ee_control3Set (Control 2 register)
	 * Control3_REG_LOOP_MASK = 0x21,
	 * Unit: unsigned char
	 * Scaling: 1
	 * @param value [in] the value that is filled into the register
	 * Range:
	 * The register value is listed in HAPTIC_ee_statusSet (Status register), HAPTIC_ee_modeSet (Mode register), HAPTIC_ee_waveformSet (Waveform register), HAPTIC_ee_goSet (Go register) and HAPTIC_ee_control3Set (Control3 register).
	 * Unit: unsigned char
	 * Scaling: 1
	 * @return int [out] Return negative integer number if there is any error.
	 * Otherwise, return 0.
	 * Range:
	 * 0: succeed to write value to register
	 * -1: fail to write value to register
	 * Unit: int
	 * Scaling: 1
	 */
	int Haptic_WriteRegBit(int fd, unsigned char reg,	unsigned char mask, unsigned char value);

	/**
	 * Given register, the function reads the value from the register.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @param reg [in] register index
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: unsigned char
	 * Scaling: 1
	 * @param reg [in] register index
	 * Range:
	 * The register name is listed in HAPTIC_ee_register.
	 * Unit: N/A
	 * Scaling: 1
	 * @return unsigned char [out] Return negative integer number if there is any error.
	 * Otherwise, return the value in the register.
	 * Range:
	 * 0 ... 255: succeed to write value to register
	 * -1: fail to read value to register
	 * Unit: unsigned char
	 * Scaling: 1
	 */
	unsigned char Haptic_ReadRegValue(int fd, unsigned char reg);

	/**
	 * Given register and mask, the function reads the register value after masking the register.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @param reg [in] register index
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: unsigned char
	 * Scaling: 1
	 * @param reg [in] register index
	 * Range:
	 * The register name is listed in HAPTIC_ee_register.
	 * Unit: N/A
	 * Scaling: 1
	 * @param mask [in] the position that is read from the register
	 * Range:
	 * HAPTIC_ee_statusSet (status register)
	 * DIAG_RESULT_MASK = (1 << 3),
	 * HAPTIC_ee_feedbackControlSet (feedboack control register)
	 * FEEDBACK_CONTROL_DEVICE_TYPE_MASK = 0x80
	 * HAPTIC_ee_memInterfaceSet (Haptic Auto Calibration Memory Interface register)
	 * AUTOCAL_MEM_INTERFACE_REG_OTP_MASK = 0x04
	 * HAPTIC_ee_control1Set (Control 1 register)
	 * Control1_REG_DRIVE_TIME_MASK = 0x1f
	 * HAPTIC_ee_control3Set (Control 2 register)
	 * Control3_REG_LOOP_MASK = 0x21,
	 * Unit: N/A
	 * Scaling: 1
	 * @return unsigned char [out] Return negative integer number if there is any error.
	 * Otherwise, return the value of the masked register.
	 * Range:
	 * 0 ... 255: succeed to write value to register
	 * -1: fail to read value to register
	 * Unit: unsigned char
	 * Scaling: 1
	 */
	unsigned char Haptic_ReadRegBit(int fd, unsigned char reg, unsigned char mask);

	/**
	 * Given register and stop condition, the function reads the register value repeatly until stop condition is satisfied.
	 *
	 * @param fd    [in] file descriptor of haptic device.
	 * Range:
	 * 1 ... man integer number: file descriptor
	 * Unit: int
	 * Scaling: 1
	 * @param reg [in] register index
	 * Range:
	 * The register name is listed in HAPTIC_ee_register.
	 * Unit: unsigned char
	 * Scaling: 1
	 * @param stopStatus [in] unsigned char value. When the function reads the value in the register, the function will stop polling.
	 * Range:
	 * The register value is listed in HAPTIC_ee_statusSet (Status register), HAPTIC_ee_modeSet (Mode register), HAPTIC_ee_waveformSet (Waveform register), HAPTIC_ee_goSet (Go register) and HAPTIC_ee_control3Set (Control3 register).
	 * Unit: N/A
	 * Scaling: 1
	 * @return None [out]
	 */
	void Haptic_PollingRegister(int fd, unsigned char reg, unsigned char stopStatus);
};
#endif // HAPTIC_H
