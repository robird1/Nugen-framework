/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList
 * Brief: 
 *
 * Create Date: 2015/5/20
 * $Revision: 25214 $
 * $Author: SteveSu $
 * $Id: EMWRList.java 25214 2015-12-01 06:18:05Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.application.emwrservice;


enum MessageList
{
    E6_E_MP_MECHANIC,
    E8_E_MP_BATTERY,
    E57_E_RC_ELECTRONIC,
    E58_E_RC_ELECTRONIC,
    E60_E_RC_RTC,
    E90_START_PUMP_FAILED,
    E91_STOP_PUMP_FAILED,
    M18_M_MP_END_OF_LIFETIME,
    M19_M_MP_RESERVOIR_LOW_FILLING,
    M21_M_MP_RESERVOIR_EMPTY,
    M22_M_MP_BATTERY_EMPTY,
    M23_M_MP_AUTOMATIC_OFF,
    M24_M_MP_OCCLUSION,
    M26_M_MP_CARTRIDGE_CHANGE,
    M27_M_MP_COMM_FAILED,
    M51_M_RC_BAD_STRIP,
    M53_M_RC_BAD_TEST,
    M54_M_RC_SAMPLE_MAINTENANCE,
    M56_M_RC_PREDOSED_STRIP,
    M58_M_RC_TEMP_LOCKOUT,
    M59_M_RC_BATTERY_EMPTY,
    M62_M_RC_BT_PAIR_FAILED,
    M64_M_RC_BOLUS_NOT_POSSIBLE_COMM,
    M65_M_RC_BOLUS_NOT_POSSIBLE_STOP, 
    M67_M_RC_BOLUS_DELIVERY_FAILED,
    M84_M_RC_TEMP_WARNING,
    M85_M_RC_PUMP_INCOMPATIBILITY,
    M86_M_RC_START_PUMP_FAILED,
    M87_M_RC_STOP_PUMP_FAILED,
    M88_M_RC_PUMP_OPERATION_FAILED_AT_FLIGHT_MODE,
    M94_M_RC_COMM_FAILED,
    M95_M_RC_BT_NEW_PAIR_FAILED,
    W25_W_MP_END_OF_LIFETIME,
    W31_W_MP_RESERVOIR_LOW,
    W32_W_MP_BATTERY_LOW, 
    W35_W_MP_BATTERY_MEDIUM,
    W36_W_MP_TBR_CANCELLED,
    W37_W_MP_LOW_DELIVERY_RATE,
    W38_W_MP_BOLUS_CANCELLED, 
    W40_W_MP_RESERVOIR_END_TIME,
    W41_W_MP_STOP_STATE,
    W50_W_RC_BATTERY_LOW,
    W71_W_RC_BA_SYNC,
    W73_W_RC_PUMP_DATAOLD,
    W75_W_RC_HYPER,
    W76_W_RC_HI,
    W80_W_RC_HYPO,
    W81_W_RC_LO,
    W82_W_RC_RADIOS_OFF,
    W84_W_RC_NO_BG_WITH_USB,
    W85_W_RC_BOLUS_ADVICE_DATA_RESET,
    W86_W_RC_BOLUS_OUT_OF_DATE,
    W88_W_RC_FLIGHTMODE_PUMP_DATAOLD,
    W89_W_RC_BA_LOGBOOK_VALUE_ZERO,
    W92_W_RC_TBR_WITH_NO_INSULIN_DELIVERY,
    W93_W_RC_READY_TO_PRIME,
    W96_W_RC_AUTOMATIC_OFF,
    W99_W_RC_BA_BATTERY_EMPTY,
    REMIDER_AFTER_HIGH_BG,
    REMIDER_AFTER_LOW_BG,
    REMIDER_AFTER_MEAL_BG,
    REMINDER_BASAL_INJECTION,
    REMINDER_BG_TEST,
    REMINDER_CHANGE_INFUSION_SET,
    REMINDER_DOCTOR_VISIT,
    REMINDER_MISSED_BOLUS,
    REMINDER_ALARM,
    REMINDER_LABTEST,
    REMINDER_CUSTOM,
    REMINDER_MICROPUMP_EXPIRY,
    
    REMIDER_AFTER_HIGH_BG_OK,
    REMIDER_AFTER_LOW_BG_OK,
    REMIDER_AFTER_MEAL_BG_OK,
    REMINDER_BASAL_INJECTION_OK,
    REMINDER_BG_TEST_OK,
    REMINDER_CHANGE_INFUSION_SET_OK,
    REMINDER_DOCTOR_VISIT_OK,
    REMINDER_MISSED_BOLUS_OK,
    REMINDER_ALARM_OK,
    REMINDER_LABTEST_OK,
    REMINDER_CUSTOM_OK;
    
}

enum ModuleList
{
	SBSP_APP, //SBSP APP
	Command_dispatcher, //Command dispatcher
	Selftest,
	SFM,
	Memory_management, //Memory management
	BTLE_service_handler, //BTLE service handler
	Connection_manager, //Connection manager
	Data_collector, //Data collector
	M10417, //10417
	M10419, //10419
	RPC,
	M10417_APP, //10417 APP
	M10419_APP, //10419 APP
	RPC_APP, //RPC APP
	UI_Command_Dispatcher, //UI Command Dispatcher
	BT_communication_Control, //BT communication Control
	Bolus,
	Basal,
	BGM_control, //BGM control
	BGM_Comm, //BGM Comm
	Settings,
	Settings_API, //Settings_API
	Startup,
	MP_replacement, //MP replacement
	USB_connetion, //USB connetion
	General_UI, //General UI
	Help_Function, //Help Function
	Quick_info, //Quick info
	screen_lock_security_lock, //screen lock/security lock
	Status_screens, //Status screens
	My_Data, //My Data
	EMWR,
	Database,
	UI_Safety_Flow_Monitoring, //UI Safety Flow Monitoring
	Safety_Class, //Safety Class
	Video_file_signature_checking, //Video file signature checking
	Utility, 
	UI_Self_Test_SW_HW, //UI Self Test (SW & HW)
	TimeManagement,
	Flight_Mode, //Flight Mode
	UI_challenge_handler, //UI challenge handler
	Reminder,
	Power_Manager, //Power Manager
	Customized_H_W_Manager, //Customized H/W Manager
	Chart_drawing, //Chart drawing
	Production_mode, //Production mode
	Configuration_Matrix, //Configuration Matrix
	Firmware_update; //Firmware update
}

public enum EMWRList
{
	//EXCEL REPORT START
	
	//FORMAT///////////////////////
	//NAME(CodeId,Message,Module,Desciption,Note)
	//FORMAT///////////////////////
	
	
	//SBSP APP	40001	40500	500
	//Command dispatcher	42501	43000	500
	//Selftest	41001	41500	500
    EMW41001( 41001, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41002( 41002, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The production item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41003( 41003, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The configuration item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41004( 41004, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The Measure-Engine item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41005( 41005, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The Verify-RTC item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41006( 41006, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The power supply test item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41007( 41007, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The insulin button item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41008( 41008, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The Comms's POST item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41009( 41009, MessageList.E58_E_RC_ELECTRONIC , ModuleList.Selftest , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
        

    //SFM	41501	42000	500
    EMW41501( 41501, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41502( 41502, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The production item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41503( 41503, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The configuration item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41504( 41504, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The Measure-Engine item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41505( 41505, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The Verify-RTC item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41506( 41506, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The power supply test item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41507( 41507, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The insulin button item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41508( 41508, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The Comms's POST item of selftest is failed." , "Hardware issue is as the same E57 error code"),
    EMW41510( 41509, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41511( 41510, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41512( 41511, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41513( 41512, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41514( 41513, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41515( 41514, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41516( 41515, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41517( 41516, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41518( 41517, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
    EMW41519( 41518, MessageList.E57_E_RC_ELECTRONIC , ModuleList.SFM , "The hardware version of selftest is failed in meter." , "Hardware issue is as the same E57 error code"),
 
	//Memory management	43001	43500	500
	//BTLE service handler	40501	41000	500
	//Connection manager	42001	42500	500
	//Data collector	43501	44000	500
	//10417	45001	45100	100
	//10419	45101	45200	100
	//RPC	45201	45300	100
	//10417 APP	45301	45400	100
	//10419 APP	45401	45500	100
	//RPC APP	45501	45600	100

	//UI Command Dispatcher	45601	45700	100
	EMW45601( 45601, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Send command error" , "" ),
	EMW45602( 45602, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Receive data header error" , "" ),
	EMW45603( 45603, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Receive data crc error" , "" ),
	EMW45604( 45604, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Receive data sequence error" , "" ),
	EMW45605( 45605, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Put request queue error" , "" ),
	EMW45606( 45606, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Take request queue error" , "" ),
	EMW45607( 45607, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Put response queue error" , "" ),
	EMW45608( 45608, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Take response queue error" , "" ),
	EMW45609( 45609, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Ack timeout error" , "" ),
	EMW45610( 45610, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Token timeout error" , "" ),
	EMW45611( 45611, MessageList.E57_E_RC_ELECTRONIC , ModuleList.UI_Command_Dispatcher , "Uart enter sleep error" , "" ),

	//BT communication Control	45701	45800	100
	EMW45701( 45701, MessageList.W73_W_RC_PUMP_DATAOLD , ModuleList.BT_communication_Control , "" , "" ),
	EMW45702( 45702, MessageList.W93_W_RC_READY_TO_PRIME , ModuleList.BT_communication_Control , "MP ready to prime" , "" ),
	EMW45703( 45703, MessageList.M62_M_RC_BT_PAIR_FAILED , ModuleList.BT_communication_Control , "MP pair failed" , "" ),
	EMW45704( 45704, MessageList.M94_M_RC_COMM_FAILED , ModuleList.BT_communication_Control , "No connection" , "" ),
	
	//Bolus	45801	45900	100
	//Basal	45901	46000	100
	EMW45901( 45901, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "current TBR is null" , "" ),
	EMW45902( 45902, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "current TBR backup is null" , "" ),
	EMW45903( 45903, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "current BRP is null" , "" ),
	EMW45904( 45904, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "current BRP backup is null" , "" ),
	EMW45905( 45905, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "current TBR number is missing" , "" ),
	EMW45906( 45906, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "current BRP number is missing" , "" ),
	EMW45907( 45907, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "More than one BRP running flags exist" , "" ),
	EMW45908( 45908, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Minus passed time from basic TBR being activated is illegal" , "" ),
	EMW45909( 45909, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "More than one TBR running flags exist" , "" ),
	EMW45910( 45910, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Minus passed time from custom TBR being activated is illegal" , "" ),
    EMW45911( 45911, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "current BRP or BRP backup is null" , "" ),
    EMW45912( 45912, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Size of time block ArrayList and its backup are not the same" , "" ),
    EMW45913( 45913, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Time blocks are not found with assigned BRP ID" , "" ),
    EMW45914( 45914, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Size of time block ArrayList is out of range" , "" ),
    EMW45915( 45915, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Integer integrity check : Two objects are the same" , "" ),
    EMW45916( 45916, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Integer integrity check : Data integrity check is failed" , "" ),
    EMW45917( 45917, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "String integrity check : Data integrity check is failed" , "" ),
    EMW45918( 45918, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Integer integrity check : Data Ch1 is not the same as data backup CH1" , "" ),
    EMW45919( 45919, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Integer integrity check : Data Ch2 is not the same as data backup CH2" , "" ),
    EMW45920( 45920, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Long integrity check : Two objects are the same" , "" ),
    EMW45921( 45921, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Long integrity check : Data Ch1 is not the same as data backup CH1" , "" ),
    EMW45922( 45922, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Long integrity check : Data Ch2 is not the same as data backup CH2" , "" ),
    EMW45923( 45923, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Long integrity check : Data integrity check is failed" , "" ),
    EMW45924( 45924, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "SafetyInteger transfered to SafetyBoolean Error" , "" ),
    EMW45925( 45925, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Unrecognized BRP action" , "" ),
    EMW45926( 45926, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Total basal from time block ArrayList is not the same as the one from time block ArrayList backup" , "" ),
    EMW45927( 45927, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Record of BRP database can't be deleted" , "" ),
    EMW45928( 45928, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "SafetyBoolean integrity check : Data is not the same as data backup" , "" ),
    EMW45929( 45929, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "SafetyString integrity check : Two objects are the same" , "" ),
    EMW45930( 45930, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "SafetyString integrity check : Data string value is not the same as data backup string value" , "" ),
    EMW45931( 45931, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "SafetyString integrity check : Data CRC is not the same as data backup CRC" , "" ),
    EMW45932( 45932, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Custom TBR number is out of range" , "" ),
    EMW45933( 45933, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Insulin LED on/off operartion error" , "" ),
    EMW45934( 45934, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Channel values check of picker value Error" , "" ),
    EMW45935( 45935, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Data is out of range" , "" ),
    EMW45936( 45936, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "TBR Storage Space Information Error" , "" ),
    EMW45937( 45937, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Bundle doesn't contain proper key and data", "" ),
    EMW45938( 45938, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "current BRP time block ID is missing", "" ),
    EMW45939( 45939, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Database query error", "" ),
    EMW45940( 45940, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Different BRP Template Code", "" ),    
    EMW45941( 45941, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Different Response Data Length", "" ),
    EMW45942( 45942, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "E2E Retry Limit Reached", "" ),
    EMW45943( 45943, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Timeout Retry Limit Reached", "" ),
    EMW45944( 45944, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Response Error", "" ),
    EMW45945( 45945, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Status Configurable Flag Error", "" ),
    EMW45946( 45946, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Command Sending Error (Cmd Write Fail)", "" ),
    EMW45947( 45947, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Command Sending Error (E2E Retry Fail)", "" ),
    EMW45948( 45948, MessageList.E57_E_RC_ELECTRONIC, ModuleList.Basal , "Latest edited TBR Not Existed", "" ),
    EMW45949( 45949, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Command Sending Error (Timeout)", "" ),    
    EMW45950( 45950, MessageList.M94_M_RC_COMM_FAILED, ModuleList.Basal , "Reset Command Sending Error", "" ),
    EMW45951( 45951, MessageList.M88_M_RC_PUMP_OPERATION_FAILED_AT_FLIGHT_MODE, ModuleList.Basal , "Reset Command Sending Error", "" ),   
	   
	//BGM control	46001	46100	100
	EMW46001( 46001, MessageList.M58_M_RC_TEMP_LOCKOUT , ModuleList.BGM_control , "Temperature out of operation range" , "Same as BGM Control's E-8 error code" ),
	EMW46002( 46002, MessageList.M84_M_RC_TEMP_WARNING , ModuleList.BGM_control , "Temperature in the warring range" , "" ),
	EMW46003( 46003, MessageList.M51_M_RC_BAD_STRIP , ModuleList.BGM_control , "Test Strip Failure" , "Same as BGM Control's E-1 error code" ),
	EMW46004( 46004, MessageList.M53_M_RC_BAD_TEST , ModuleList.BGM_control , "Test Unsuccessful" , "Same as BGM Control's E-3 error code" ),
	EMW46005( 46005, MessageList.W76_W_RC_HI , ModuleList.BGM_control , "HI bg value" , "" ),
	EMW46006( 46006, MessageList.W75_W_RC_HYPER , ModuleList.BGM_control , "hyper bg value" , "" ),
	EMW46007( 46007, MessageList.W80_W_RC_HYPO , ModuleList.BGM_control , "hypo bg value" , "" ),
	EMW46008( 46008, MessageList.W81_W_RC_LO , ModuleList.BGM_control , "LO bg" , "" ),
	EMW46009( 46009, MessageList.W84_W_RC_NO_BG_WITH_USB , ModuleList.BGM_control , "USB plug in when the bg measurement" , "" ),
	EMW46010( 46010, MessageList.M54_M_RC_SAMPLE_MAINTENANCE , ModuleList.BGM_control , "Drop too small" , "Same as BGM Control's E-4 error code" ),
	EMW46011( 46011, MessageList.M56_M_RC_PREDOSED_STRIP , ModuleList.BGM_control , "Dorp too soon" , "Same as BGM Control's E-6 error code" ),
	EMW46012( 46012, MessageList.E57_E_RC_ELECTRONIC , ModuleList.BGM_control , "hardware error" , "Same as BGM Control's E-7 error code" ),
	EMW46013( 46013, MessageList.W71_W_RC_BA_SYNC , ModuleList.BGM_control , "Stop RF fail" , "" ),
	
	//BGM Comm	46101	46200	100
	//Settings	46201	46300	100
	//Settings API	46301	46400	100
	EMW46301( 46301, MessageList.E57_E_RC_ELECTRONIC , ModuleList.Settings_API , "Parameter error" , "" ),
	EMW46302( 46302, MessageList.E57_E_RC_ELECTRONIC , ModuleList.Settings_API , "Exception occurred" , "" ),
	
	//Startup	46401	46500	100
	//MP replacement	46501	46600	100
	//USB connetion	46601	46700	100
	EMW46601( 46601, MessageList.E57_E_RC_ELECTRONIC , ModuleList.USB_connetion , "hardware error" , "Error from PCConnect.java" ),
	EMW46602( 46602, MessageList.E57_E_RC_ELECTRONIC , ModuleList.USB_connetion , "hardware error" , "Error from UsbConnectReceiver.java" ),
	EMW46603( 46603, MessageList.E57_E_RC_ELECTRONIC , ModuleList.USB_connetion , "hardware error" , "Error from SCR0003_connect_to_pc_eject.java" ),
	
	//General UI	46701	46800	100
	//Help Function	46801	46900	100
	//Quick info	46901	47000	100
	//screen lock/security lock	47001	47100	100
	//Status screens	47101	47200	100
	//My Data	47201	47300	100
	//EMWR	47301	47400	100

    EMW47301( 47301, MessageList.E57_E_RC_ELECTRONIC , ModuleList.EMWR , "Fail to bind EMWR" , "" ),
    EMW47302( 47302, MessageList.E60_E_RC_RTC , ModuleList.EMWR , "Message for EMWR unit test" , "" ),
    EMW47303( 47303, MessageList.E6_E_MP_MECHANIC , ModuleList.EMWR , "Message for EMWR unit test" , "" ),
    EMW47304( 47304, MessageList.E8_E_MP_BATTERY , ModuleList.EMWR , "Message for EMWR unit test" , "" ),
    EMW47305( 47305, MessageList.E57_E_RC_ELECTRONIC , ModuleList.EMWR , "Fail to log information." , "" ),
    EMW47306( 47306, MessageList.E57_E_RC_ELECTRONIC , ModuleList.EMWR , "The direcotries do not exist." , "" ),
	
	//Database	47401	47500	100
	//UI Safety Flow Monitoring	47501	47600	100
	//Safety Class	47601	47700	100
	//Video file signature checking	47701	47800	100
	//Utility 	47801	47900	100
	EMW47801( 47801, MessageList.E57_E_RC_ELECTRONIC , ModuleList.Utility , "hardware error" , "Error from CommonUtils.java" ),
	
	//UI Self Test (SW & HW)	47901	48000	100
	//TimeManagement	48001	48100	100
    EMW48001(48001, MessageList.E60_E_RC_RTC, ModuleList.TimeManagement, "BGM time is out of sync.",""),
    EMW48002(48002, MessageList.E60_E_RC_RTC, ModuleList.TimeManagement, "Pump time is out of sync.","It has been reset automatically"),
    EMW48003(48003, MessageList.E60_E_RC_RTC, ModuleList.TimeManagement, "Setting date/time is out of range.",""),
    EMW48004(48004, MessageList.E60_E_RC_RTC, ModuleList.TimeManagement, "Setting date/time shall not be earlier then manufacturing date.",""),
	//Flight Mode	48101	48200	100
    EMW48101(48101, MessageList.M88_M_RC_PUMP_OPERATION_FAILED_AT_FLIGHT_MODE, ModuleList.Flight_Mode, "Micropump unable to complete task",""),
    EMW48102(48102, MessageList.W89_W_RC_BA_LOGBOOK_VALUE_ZERO, ModuleList.Flight_Mode, "Bolus warning caused by bolus to zero",""),
      
	//UI challenge handler	48201	48300	100
	//Reminder	48301	48400	100
    EMW48301(48301, MessageList.REMINDER_CHANGE_INFUSION_SET, ModuleList.Reminder, "Change Infusion Set", ""),
    EMW48302(48302, MessageList.REMINDER_BG_TEST, ModuleList.Reminder, "bG Test", ""),
    EMW48303(48303, MessageList.REMINDER_MISSED_BOLUS, ModuleList.Reminder, "Missed Bolus", ""),
    EMW48304(48304, MessageList.REMINDER_BASAL_INJECTION, ModuleList.Reminder, "Basal Injection", ""),
    EMW48305(48305, MessageList.REMIDER_AFTER_HIGH_BG, ModuleList.Reminder, "bG After High", ""),
    EMW48306(48306, MessageList.REMIDER_AFTER_LOW_BG, ModuleList.Reminder, "bG After Low", ""),
    EMW48307(48307, MessageList.REMIDER_AFTER_MEAL_BG, ModuleList.Reminder, "bG After Meal", ""),
    EMW48308(48308, MessageList.REMINDER_DOCTOR_VISIT, ModuleList.Reminder, "Doctor Visit", ""),
    EMW48309(48309, MessageList.REMINDER_LABTEST, ModuleList.Reminder, "Lab Test", ""),
    EMW48310(48310, MessageList.REMINDER_ALARM, ModuleList.Reminder, "Alarm Clock", ""),
    EMW48311(48311, MessageList.REMINDER_CUSTOM, ModuleList.Reminder, "Custom", ""),
    
    EMW48312(48312, MessageList.REMINDER_CHANGE_INFUSION_SET_OK, ModuleList.Reminder, "Change Infusion Set with OK", ""),
    EMW48313(48313, MessageList.REMINDER_BG_TEST_OK, ModuleList.Reminder, "bG Test with OK", ""),
    EMW48314(48314, MessageList.REMINDER_MISSED_BOLUS_OK, ModuleList.Reminder, "Missed Bolus with OK", ""),
    EMW48315(48315, MessageList.REMINDER_BASAL_INJECTION_OK, ModuleList.Reminder, "Basal Injection with OK", ""),
    EMW48316(48316, MessageList.REMIDER_AFTER_HIGH_BG_OK, ModuleList.Reminder, "bG After High with OK", ""),
    EMW48317(48317, MessageList.REMIDER_AFTER_LOW_BG_OK, ModuleList.Reminder, "bG After Low with OK", ""),
    EMW48318(48318, MessageList.REMIDER_AFTER_MEAL_BG_OK, ModuleList.Reminder, "bG After Meal with OK", ""),
    EMW48319(48319, MessageList.REMINDER_DOCTOR_VISIT_OK, ModuleList.Reminder, "Doctor Visit with OK", ""),
    EMW48320(48320, MessageList.REMINDER_LABTEST_OK, ModuleList.Reminder, "Lab Test with OK", ""),
    EMW48321(48321, MessageList.REMINDER_ALARM_OK, ModuleList.Reminder, "Alarm Clock with OK", ""),
    EMW48322(48322, MessageList.REMINDER_CUSTOM_OK, ModuleList.Reminder, "Custom with OK", ""),

	//Power Manager	48401	48500	100
    
	EMW48401( 48401, MessageList.M84_M_RC_TEMP_WARNING , ModuleList.Power_Manager , "Temperature is out of operation range" , "" ),
	EMW48402( 48402, MessageList.W50_W_RC_BATTERY_LOW , ModuleList.Power_Manager , "Battery is Low" , "" ),
	EMW48403( 48403, MessageList.M59_M_RC_BATTERY_EMPTY , ModuleList.Power_Manager , "Battery is Empty" , "" );
	//Customized H/W Manager	48501	48600	100
	//Chart drawing	48601	48700	100
	//Production mode	48701	48800	100
	//Configuration Matrix	48801	48900	100
	//Firmware update	48901	49000	100
    
	
	//EXCEL REPORT END
	
	private final int mCodeId;
    private final MessageList mMessage;
    private final ModuleList mModule;
    private final String mDesciption;
    private final String mNote;

    
    
    /**
     * enum constructor
     * 
     * @param CodeId [in] int internal code of an EMW.
     * @param Message [in] MessageList coresspond dialog of an EMW.
     * @param Module [in] ModuleList module name of an EMW.
     * @param Desciption [in] String content of an EMW.
     * @param Note [in] String misc information of an EMW.
     * 
     * return None
     * 
     */
    private EMWRList(
    		int CodeId,MessageList Message, ModuleList Module, String Desciption, String Note)
    {
    	mCodeId = CodeId;
    	mMessage = Message;
    	mModule = Module;
    	mDesciption = Desciption;
    	mNote = Note;
    }
    
    /**
     * Member of EMWRList.
     * 
     * @param none.
     * @return int [out] defined internal code.
     */
    public int getCodeId(){
    	return mCodeId;
    }
    
    /**
     * Member of EMWRList.
     * 
     * @param none.
     * @return String [out] defined dialogbox.
     */
    public String toString(){
    	return mMessage.toString();
    }
    
    /**
     * Member of EMWRList.
     * 
     * @param none.
     * @return String [out] defined description.
     */
    public String getDesciption()
    {
        return mDesciption.toString();
    }
    
    public static EMWRList fromCode(int code)
    {
        EMWRList object = EMWRList.EMW48311;
        
        for (EMWRList type : EMWRList.values())
        {
            int temp = type.getCodeId();
            if (temp == code)
            {
                object = type;
                
                break;
            }
        }
        
        return object;
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// (R15066 2015-08-20 01:37:40 KayjeanKu)
// ----------------------------------------------------------------------------
// [NISQ-20] Add Error Handler
// (R15209 2015-08-22 23:31:51 henrytso)
// ----------------------------------------------------------------------------
// Add basal error codes
// (R15464 2015-08-25 22:40:18 JamesLee)
// ----------------------------------------------------------------------------
// 1. Move loadSound() & playSound() to CommonUtils.java
// 2. Add errorHandler() to CommonUtils.java
// (R15571 2015-08-27 03:03:02 WillLong)
// ----------------------------------------------------------------------------
// Update Basal Delivery Error Code
// (R15938 2015-08-31 23:13:38 KayjeanKu)
// ----------------------------------------------------------------------------
// Add Error code of Basal
// (R23329 2015-11-05 01:51:41 TerryHsieh)
// ----------------------------------------------------------------------------
// [New Feature] Basal Delivery comms functions
// (R24137 2015-11-13 07:00:18 JacksonHuang)
// ----------------------------------------------------------------------------
// [Update] Modify comments
// [Update] Add WaitDelivery for long time delivery time
// (R24331 2015-11-18 03:40:12 JamesLee)
// ----------------------------------------------------------------------------
// [Reminder] add screen - missed bolus reminder, basal injection reminder
// (R24347 2015-11-18 05:17:48 SteveSu)
// ----------------------------------------------------------------------------
// [Reminder] 1. add bG after high / low / meal reminders
// 2. add doctor visit and lab test reminders
// (R24549 2015-11-20 06:07:24 SteveSu)
// ----------------------------------------------------------------------------
// [Reminder] add Alarm Clock / Custom reminders
// (R24896 2015-11-25 23:51:28 JamesLee)
// ----------------------------------------------------------------------------
// [Reminder] update Reminder module
// (R24896 2015-11-25 23:51:28 JamesLee)
// ----------------------------------------------------------------------------
// [Reminder] update Reminder module
// (R24896 2015-11-25 23:51:28 JamesLee)
// ----------------------------------------------------------------------------
// [Reminder] update Reminder module
