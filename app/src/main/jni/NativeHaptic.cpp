// DESCRIPTION: Haptic Service
//
//
//*****************************************************************************
// UPDATE LOG:
// $Log: $
//*****************************************************************************
//****************** STANDARD CPP-INCLUDE FILES *********************************
//************* GLOBAL AND PROJECT INCLUDE FILES ******************************
#include <NativeHaptic.h>

#define LOG_NDEBUG 0

//This macro is used to switch show/disable logcat
#define LOG_TAG "nativeHapticService"

//haptic device configure
Haptic_st_Actuator actuator =
{
	.device_type = ERM,
	.g_effect_bank = LIBRARY_B,
	.loop = CLOSE_LOOP,
	.rated_vol = 0x3d,
	.over_drive_vol = 0x4d,
	.drive_time = 0x13,
};

/**
 * Get NativeHaptic instance for implementing Singleton pattern.
 * 
 * @param None    [in]
 * @return None [out]
 */
 NativeHaptic* NativeHaptic::getInstance()
 {
	static NativeHaptic *pInstance = new NativeHaptic;

	return pInstance;
}

/**
 * NativeHaptic Constructor does nothing here, but protecting the constructor
 * meets Sigleton pattern.
 * 
 * If OTP is 0, reset registers (RATED_VOLTAGE_REG/ OVERDRIVE_CLAMP_VOLTAGE_REG/
 * FEEDBACK_CONTROL_REG/ Control1_REG/ Control3_REG) according to
 * Haptic_st_Actuator variable.
 * If OTP is not 0, just reset control registers (Control1_REG and Control3_REG).
 * 
 * Finally, set MODE_REG MODE_STANDBY.
 * 
 * @param None    [in]
 * @return None [out]
 */
NativeHaptic::NativeHaptic()
{
	ALOGD("[%s]++ \n", __FUNCTION__);

	int error = -1;

	//haptic device fd
	int fd = -1;

	//initial haptic device
	fd = Haptic_OpenDevice(HAPTIC_PATH, HAPTIC_SLAVE_BASE_ADDRESS);

	if (fd >= 0)
	{
		int value = -1;

		Haptic_WriteRegValue(fd, MODE_REG, MODE_INTERNAL_TRIGGER);

		value = Haptic_ReadRegBit(fd, AUTOCAL_MEM_INTERFACE_REG,
						AUTOCAL_MEM_INTERFACE_REG_OTP_MASK);

		ALOGD("[%s]: OTP = %d\n", __FUNCTION__, value);

		//select lib
		Haptic_WriteRegValue(fd, LIBRARY_SELECTION_REG, actuator.g_effect_bank);

		if (value == 0)
		{
			Haptic_ResetRegBySetting(fd, actuator);

//			Haptic_AutoCalibration(fd);
//			Haptic_Diagnostic(fd);
		}
		else
		{
			ALOGD("%s, OTP programmed\n", __FUNCTION__);

			Haptic_SetControlBySetting(fd, actuator);
		}

		/* Put hardware in standby */
		Haptic_WriteRegValue(fd, MODE_REG, MODE_STANDBY);

		close(fd);

		error = 0;
	}
	else
	{
		ALOGD("[%s], open haptic ERROR\n", __FUNCTION__);
		error = -1;
	}

	ALOGD("[%s]-- \n", __FUNCTION__);
}

/**
 * NativeHaptic Destructor that does nothing here.
 * 
 * @param None    [in]
 * @return None [out]
 */
NativeHaptic::~NativeHaptic()
{

}

/**
 * Play specific haptic type, 0x0f.
 * 
 * @param None    [in]
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * 0: succeed to play default vibration.
 * -1: fail to play default vibration.
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_PlayOnce()
{
	ALOGD("[%s]++ \n", __FUNCTION__);

    int error = -1;

	//haptic device fd
	int fd = -1;

	 //initial haptic device
    fd = Haptic_OpenDevice(HAPTIC_PATH, HAPTIC_SLAVE_BASE_ADDRESS);

    if (fd >= 0)
    {
    	error = Haptic_WriteRegValue(fd, MODE_REG, MODE_INTERNAL_TRIGGER);

		//Waveform Sequencer
    	error |= Haptic_WriteRegValue(fd, WAVEFORM_SEQUENCER_REG, WAVEFORM_SEQUENCER_REG_VALUE);

		//Used to fire processes in the DRV2605
    	error |= Haptic_WriteRegValue(fd, GO_REG, GO);

		Haptic_PollingRegister(fd, GO_REG, STOP);

		error |= Haptic_WriteRegValue(fd, MODE_REG, MODE_STANDBY);

		close(fd);
    }
    else
    {
    	ALOGD("[%s], open haptic ERROR\n", __FUNCTION__);
    }

    ALOGD("[%s]-- \n", __FUNCTION__);

    return error;
}

/**
 * Stop haptic.
 * 
 * @param None    [in]
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * 0: succeed to close vibration.
 * -1: fail to close vibration.
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_Stop()
{
	ALOGD("[%s]\n", __FUNCTION__);

	int error = -1;

	//haptic device fd
	int fd = -1;

	//initial haptic device
    fd = Haptic_OpenDevice(HAPTIC_PATH, HAPTIC_SLAVE_BASE_ADDRESS);

    if (fd >= 0)
    {
    	error = Haptic_WriteRegValue(fd, GO_REG, STOP);
    	error |= Haptic_WriteRegValue(fd, MODE_REG, MODE_STANDBY);

    	close(fd);
    }
    else
    {
    	ALOGD("[%s], open haptic ERROR\n", __FUNCTION__);
    }

    return error;
}

/**
 * Given an haptic type, play it by the type.
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * 0: succeed to play vibration.
 * -1: fail to play vibration.
 * Unit: N/A
 * Scaling: 1
 * 
 * @param index    [in] unsigned char pointer that is specific haptic type in the
 * driver.
 * The range is from 1 to 123.
 * Range:
 * 1 ... 123
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_Play(unsigned char *index)
{
    ALOGD("[%s]\n", __FUNCTION__);

	int error = -1;
	int fd = -1;        //haptic device fd

	//initial haptic device
    fd = Haptic_OpenDevice(HAPTIC_PATH, HAPTIC_SLAVE_BASE_ADDRESS);

    if(fd >= 0)
    {
    	//Range Check avalible index: 1~123
		if((*index > 0) && (*index < 124))
		{
			ALOGD("[%s]: index = %d\n", __FUNCTION__, *index);

			error = Haptic_WriteRegValue(fd, MODE_REG, MODE_INTERNAL_TRIGGER);
			error |= Haptic_WriteRegValue(fd, WAVEFORM_SEQUENCER_REG, *index);
			error |= Haptic_WriteRegValue(fd, GO_REG, GO);

			Haptic_PollingRegister(fd, GO_REG, STOP);

			error |= Haptic_WriteRegValue(fd, MODE_REG, MODE_STANDBY);
		}

		close(fd);
    }
    else
    {
    	ALOGD("[%s], open haptic ERROR\n", __FUNCTION__);
    }

    return error;
}

/**
 * Calibrate Haptic according calibration sequence.
 * The sequence is
 * 1. Set MODE_REG to AUTO_CALIBRATION.
 * 2. Set REAL_TIME_PLAYBACK_REG to REAL_TIME_PLAYBACK_STRENGTH.
 * 3. Set GO_REG to GO.
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * -1: fail to haptic calibration
 * 0: succeed to haptic calibration
 * Unit: N/A
 * Scaling: 1
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_AutoCalibration(int fd)
{
    ALOGD("[%s]\n", __FUNCTION__);

    int error = -1;
	const int SUCCESS_INDEX = RETRY_TIMES + 100;
	unsigned char autoCaliResult = '\0';
	unsigned char autoCaliBackEmfResult = '\0';
	unsigned char feedBackControlReg = '\0';

	for(int i = 0; i < RETRY_TIMES; i++)
	{
		error = Haptic_WriteRegValue(fd, HAPTIC_calibrationSequence, sizeof(HAPTIC_calibrationSequence));

		if(error >= 0)
		{
			/* Wait until the procedure is done */
			Haptic_PollingRegister(fd, GO_REG, STOP);

			/* Read status */
			error = Haptic_ReadRegBit(fd, STATUS_REG, DIAG_RESULT_MASK);

			/* Check result */
			if (error != AUTO_CAL_FAILED)
			{
				i = SUCCESS_INDEX;
			}
			else
			{
				ALOGD("drv260x auto-cal retry failed.\n");
			}
		}
	}

	/* Read calibration results */
	autoCaliResult = Haptic_ReadRegValue(fd, AUTO_CALI_RESULT_REG);
	autoCaliBackEmfResult = Haptic_ReadRegValue(fd, AUTO_CALI_BACK_EMF_RESULT_REG);
	feedBackControlReg = Haptic_ReadRegValue(fd, FEEDBACK_CONTROL_REG);

	ALOGD("[%s]: autoCaliResult = 0x%x\n", __FUNCTION__, autoCaliResult);
	ALOGD("[%s]: autoCaliBackEmfResult = 0x%x\n", __FUNCTION__, autoCaliBackEmfResult);
	ALOGD("[%s]: feedBackControlReg = 0x%x\n", __FUNCTION__, feedBackControlReg);

	return error;
}

/**
 * Diagnose Haptic according diagnostic Sequence.
 * The sequence is
 * 1. Set MODE_REG MODE_DIAGNOSTICS.
 * 2. Set GO_REG GO.
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * -1: fail to haptic diagnostic
 * 0: succeed to haptic diagnostic
 * Unit: N/A
 * Scaling: 1
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_Diagnostic(int fd)
{
    ALOGD("[%s]++ \n", __FUNCTION__);

	int error = 0;
	unsigned char diagResult = '\0';

	error = Haptic_WriteRegValue(fd, HAPTIC_diagnosticSequence, sizeof(HAPTIC_diagnosticSequence));

	if(error >= 0)
	{
		/* Wait until the procedure is done */
		Haptic_PollingRegister(fd, GO_REG, STOP);

		/* Read result */
		error = Haptic_ReadRegBit(fd, STATUS_REG, DIAG_RESULT_MASK);

		ALOGD("[%s]: diagResult = %d\n", __FUNCTION__, error);
	}

	ALOGD("[%s]-- \n", __FUNCTION__);

	return error;

}

/**
 * Open Haptic device and return its file descriptor.
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return file descriptor.
 * Range:
 * 0: succeed to open vibration device.
 * -1: fail to open vibration device.
 * Unit: N/A
 * Scaling: 1
 * 
 * @param path    [in] String of the Haptic device path.
 * Range:
 * HAPTIC_PATH: "/dev/i2c-1"
 * Unit: N/A
 * Scaling: 1
 * @param addr    [in] Haptic device's address
 * Range:
 * HAPTIC_SLAVE_BASE_ADDRESS: 0x5a.
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_OpenDevice(const char* path, unsigned int addr)
{
	ALOGD("[%s] ++\n", __FUNCTION__);

	NativeI2CControl *i2cControl = NativeI2CControl::getInstance();
	int fd = -1;

	fd = i2cControl->HapticI2C_OpenDevice(path, addr);

	ALOGD("[%s] --\n", __FUNCTION__);

	return fd;
}

/**
 * Set registers, RATED_VOLTAGE_REG/ OVERDRIVE_CLAMP_VOLTAGE_REG/
 * FEEDBACK_CONTROL_REG.
 * 
 * @return None [out]
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 * @param setting    [in] Haptic_st_Actuator variables that includes registers'
 * values.
 * Range:
 * device_type: ERM or LRA in HAPTIC_ee_actuatorType
 * g_effect_bank: LIBRARY_B
 * loop: OPEN_LOOP or CLOSE_LOOP in HAPTIC_ee_loopType
 * rated_vol: 0x3d
 * over_drive_vol: 0x4d
 * drive_time: 0x13
 * Unit: N/A
 * Scaling: 1
 */
void NativeHaptic::Haptic_ResetRegBySetting(int fd, Haptic_st_Actuator setting)
{
	ALOGD("[%s] ++\n", __FUNCTION__);

	//set RATED_VOLTAGE_REG, OVERDRIVE_CLAMP_VOLTAGE_REG
	if (setting.rated_vol != 0)
	{
		ALOGD("%s, RatedVol = 0x%x\n", __FUNCTION__, setting.rated_vol);
		Haptic_WriteRegValue(fd, RATED_VOLTAGE_REG, setting.rated_vol);
	}
	else
	{
		ALOGD("%s, ERROR Rated ZERO\n", __FUNCTION__);
	}

	if (setting.over_drive_vol != 0)
	{
		ALOGD("%s, OverDriveVol = 0x%x\n", __FUNCTION__, setting.over_drive_vol);
		Haptic_WriteRegValue(fd, OVERDRIVE_CLAMP_VOLTAGE_REG,
				setting.over_drive_vol);
	}
	else
	{
		ALOGD("%s, ERROR OverDriveVol ZERO\n", __FUNCTION__);
	}

	Haptic_WriteRegBit(fd, FEEDBACK_CONTROL_REG,
				FEEDBACK_CONTROL_DEVICE_TYPE_MASK, setting.device_type);

	Haptic_SetControlBySetting(fd, setting);

	ALOGD("[%s] --\n", __FUNCTION__);
}

/**
 * Set registers, Control1_REG and Control3_REG.
 * 
 * @return None [out]
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 * @param setting    [in] Haptic_st_Actuator variables that includes registers' values.
 * Range:
 * device_type: ERM or LRA in HAPTIC_ee_actuatorType
 * g_effect_bank: LIBRARY_B
 * loop: OPEN_LOOP or CLOSE_LOOP in HAPTIC_ee_loopType
 * rated_vol: 0x3d
 * over_drive_vol: 0x4d
 * drive_time: 0x13
 * Unit: N/A
 * Scaling: 1
 */
void NativeHaptic::Haptic_SetControlBySetting(int fd, Haptic_st_Actuator setting)
{
	ALOGD("[%s] ++\n", __FUNCTION__);

	unsigned char loop = 0;

	//set drive_time
	if (setting.drive_time != DEFAULT_DRIVE_TIME)
	{
		Haptic_WriteRegBit(fd, Control1_REG, Control1_REG_DRIVE_TIME_MASK,
				setting.drive_time);
	}

	//set loop
	if (setting.loop == OPEN_LOOP)
	{
		if (setting.device_type == LRA)
		{
			loop = LRA_OpenLoop_Enabled;
		}
		else if (setting.device_type == ERM)
		{
			loop = ERM_OpenLoop_Enabled;
		}
	}
	else
	{
		loop = 0;
	}

	Haptic_WriteRegBit(fd, Control3_REG, Control3_REG_LOOP_MASK, loop);

	ALOGD("[%s] --\n", __FUNCTION__);
}


/**
 * Given register and value, the function writes the value into the register.
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * 0: succeed to write value to register
 * -1: fail to write value to register
 * Unit: N/A
 * Scaling: 1
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 * @param reg    [in] register index
 * Range: The register name is listed in HAPTIC_ee_register.
 * Unit: N/A
 * Scaling: 1
 * @param value    [in] the value that is filled into the register
 * Range:
 * The register value is listed in HAPTIC_ee_statusSet (Status register), HAPTIC_ee_modeSet (Mode register), HAPTIC_ee_waveformSet (Waveform register), HAPTIC_ee_goSet (Go register) and HAPTIC_ee_control3Set (Control3 register).
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_WriteRegValue(int fd, unsigned char reg, unsigned char value)
{
	int error = 0;
	NativeI2CControl *control = NativeI2CControl::getInstance();

	error = control->HapticI2C_WriteRegister(fd, reg, value);

	if (error < 0)
	{
		ALOGE("%s, err=%d\n", __FUNCTION__, error);
	}

	return error;
}

/**
 * Given the array of registers and values, the function writes each value into
 * each register.
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * 0: succeed to write value to register
 * -1: fail to write value to register
 * Unit: N/A
 * Scaling: 1
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 * @param data    [in] unsigned char pointer that points an array.
 * The format of the array is {register_name, register_value, ....}.
 * register_name Range: (in HAPTIC_ee_register)
 * The register name is listed in HAPTIC_ee_register.
 * Unit: N/A
 * Scaling: 1
 * register_value Range:
 * The register value is listed in HAPTIC_ee_statusSet (Status register), HAPTIC_ee_modeSet (Mode register), HAPTIC_ee_waveformSet (Waveform register), HAPTIC_ee_goSet (Go register) and HAPTIC_ee_control3Set (Control3 register).
 * Unit: N/A
 * Scaling: 1
 * @param size    [in] the size of data
 * Range:
 * 2 ... max unsigned int that is multiplies of 2
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_WriteRegValue(int fd, const unsigned char* data, unsigned int size)
{
	int error = 0;

	if ((size % 2) != 0)
	{
		error = -EINVAL;
	}
	else
	{
		unsigned int i = 0;
		const unsigned int STOP_NUMBER = size + 100;

		while (i < size)
		{
			NativeI2CControl *control = NativeI2CControl::getInstance();

			error = control->HapticI2C_WriteRegister(fd, data[i], data[i + 1]);

			if (error < 0)
			{
				ALOGE("%s, err=%d\n", __FUNCTION__, error);
				i = STOP_NUMBER;
			}
			else
			{
				i += 2;
			}
		}
	}

	return error;
}

/**
 * Given register, mask and value, the function writes the value into the position
 * after masking the register .
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * 0: succeed to write value to register
 * -1: fail to write value to register
 * Unit: N/A
 * Scaling: 1
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 * @param reg    [in] register index
 * Range:
 * The register name is listed in HAPTIC_ee_register.
 * Unit: N/A
 * Scaling: 1
 * @param mask    [in] the position that is filled in the register
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
 * @param value    [in] the value that is filled into the register
 * Range:
 * The register value is listed in HAPTIC_ee_statusSet (Status register), HAPTIC_ee_modeSet (Mode register), HAPTIC_ee_waveformSet (Waveform register), HAPTIC_ee_goSet (Go register) and HAPTIC_ee_control3Set (Control3 register).
 * Unit: N/A
 * Scaling: 1
 */
int NativeHaptic::Haptic_WriteRegBit(int fd, unsigned char reg,	unsigned char mask, unsigned char value)
{
	unsigned char error = 0;
	unsigned char temp = 0;
	unsigned char buff[2];
	NativeI2CControl *control = NativeI2CControl::getInstance();
	unsigned char regval;

	error = control->HapticI2C_ReadRegister(fd, reg, &regval);

	temp = regval & ~mask;
	temp |= value & mask;

	if (temp != regval)
	{
		error = control->HapticI2C_WriteRegister(fd, reg, temp);
	}
	else
	{
		ALOGV("Setting is existed.");
	}

	return error;
}

/**
 * Given register, the function reads the value from the register.
 * 
 * @return unsigned char [out] Return negative integer number if there is any
 * error.
 * Otherwise, return the value in the register.
 * Range:
 * 0 ... 255: succeed to write value to register
 * -1: fail to read value to register
 * Unit: N/A
 * Scaling: 1
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 * @param reg    [in] register index
 * Range:
 * The register name is listed in HAPTIC_ee_register.
 * Unit: N/A
 * Scaling: 1
 */
unsigned char NativeHaptic::Haptic_ReadRegValue(int fd, unsigned char reg)
{
	unsigned char error = 0;
	unsigned char regval;
	NativeI2CControl *control = NativeI2CControl::getInstance();

	error = control->HapticI2C_ReadRegister(fd, reg, &regval);

	if(error >= 0)
	{
		error = regval;
	}

	return error;
}

/**
 * Given register and mask, the function reads the register value after masking
 * the register.
 * 
 * @return unsigned char [out] Return negative integer number if there is any
 * error.
 * Otherwise, return the value of the masked register.
 * Range:
 * 0 ... 255: succeed to write value to register
 * -1: fail to read value to register
 * Unit: N/A
 * Scaling: 1
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 * @param reg    [in] register index
 * Range:
 * The register name is listed in HAPTIC_ee_register.
 * Unit: N/A
 * Scaling: 1
 * @param mask    [in] the position that is read from the register
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
 */
unsigned char NativeHaptic::Haptic_ReadRegBit(int fd, unsigned char reg, unsigned char mask)
{
	unsigned char error = 0;
	unsigned char temp = 0;
	unsigned char regval;
	NativeI2CControl *control = NativeI2CControl::getInstance();

	error = control->HapticI2C_ReadRegister(fd, reg, &regval);

	if(error >= 0)
	{
		temp = regval & mask;
		error = temp;
	}

	return error;
}

/**
 * Given register and stop condition, the function reads the register value
 * repeatly until stop condition is satisfied.
 * 
 * @return None [out]
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: 1
 * @param reg    [in] register index
 * Range:
 * The register name is listed in HAPTIC_ee_register.
 * Unit: N/A
 * Scaling: 1
 * @param stopStatus    [in] unsigned char value. When the function reads the
 * value in the register, the function will stop polling.
 * Range:
 * The register value is listed in HAPTIC_ee_statusSet (Status register), HAPTIC_ee_modeSet (Mode register), HAPTIC_ee_waveformSet (Waveform register), HAPTIC_ee_goSet (Go register) and HAPTIC_ee_control3Set (Control3 register).
 * Unit: N/A
 * Scaling: 1
 */
void NativeHaptic::Haptic_PollingRegister(int fd, unsigned char reg, unsigned char stopStatus)
{
    int isFind = 0;
    int tempRegValue = -1;

    while(isFind == 0)
    {
    	tempRegValue = Haptic_ReadRegValue(fd, reg);

        if(tempRegValue == stopStatus)
        {
             isFind = 1;
        }
    }
}




