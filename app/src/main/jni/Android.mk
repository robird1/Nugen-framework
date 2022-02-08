#
# This makefile supplies the rules for building a library of JNI code for
# use by our example platform shared library.

LOCAL_PATH:= $(call my-dir)
include $(CLEAR_VARS)

# LOCAL_CFLAGS   := -fsanitize=address -fno-omit-frame-pointer
# LOCAL_LDFLAGS  := -fsanitize=address
# LOCAL_ARM_MODE := arm

# LD_PRELOAD=libclang_rt.asan-arm-android.so

# [optional, user, eng] 
# eng = required
# optinal = no install on target
LOCAL_MODULE_TAGS := optional

# customize header path
CUST_H_INCLUDE := $(LOCAL_PATH)/inc \

# LOCAL_ALLOW_UNDEFINED_SYMBOLS:=true

LOCAL_LDLIBS := -llog -ldl

# This is the target being built.
LOCAL_MODULE:= libcustframeworksvr_jni

# Target install path.
LOCAL_MODULE_PATH := $(TARGET_OUT_SHARED_LIBRARIES)

# Need bt api source
LOCAL_BT_JPAK_SRC := \
            jpake.c \
			sha_256.c \
			ecc.c \
			ecc_curves32.c
			
LOCAL_BT_RANDOM_SRC := \
			hash_drbg.c
    				   

# Need uart api source
LOCAL_UART_SRC := \
	NativeUART.cpp \
	NativeUICP.cpp
	
# Need led api source
LOCAL_LED_SRC := \
	NativeLED.cpp
	
# Need haptic api source
LOCAL_HAPTIC_SRC := \
	NativeI2CControl.cpp \
	NativeHaptic.cpp

# All of the source files that we will compile.
LOCAL_SRC_FILES:= \
    $(LOCAL_COMMS_SRC) \
    $(LOCAL_UART_SRC) \
    $(LOCAL_LED_SRC) \
    $(LOCAL_HAPTIC_SRC) \
    $(LOCAL_BT_JPAK_SRC) \
    $(LOCAL_BT_RANDOM_SRC) \
	custHal.cpp \
	CustRunTime.cpp \
	com_accu_chek_solo_m_rcapp_application_CustUEventObserver.cpp \
	com_accu_chek_solo_m_rcapp_application_FWUpdateUartService.cpp \
	com_accu_chek_solo_m_rcapp_communication_uicommanddispatcher_CommsJNI.cpp \
	com_accu_chek_solo_m_rcapp_communication_uartuicp_UICP.cpp	\
	com_accu_chek_solo_m_rcapp_application_BgmHWControl.cpp \
	com_accu_chek_solo_m_rcapp_application_customizedhwmanager_ledcontrol_LED.cpp \
	com_accu_chek_solo_m_rcapp_application_customizedhwmanager_vibrationcontrol_Haptic.cpp \
	com_accu_chek_solo_m_rcapp_application_customizedhwmanager_adcreader_ADC.cpp \
	com_accu_chek_solo_m_rcapp_application_ble_blemanager_KeyExchagneJNI.cpp \
	com_accu_chek_solo_m_rcapp_communication_customizedhwmanager_uartinterface_UART.cpp 

# All of the shared libraries we link against.
LOCAL_SHARED_LIBRARIES := \
	libandroid_runtime \
	libnativehelper \
	libcutils \
	libutils

# No static libraries.
LOCAL_STATIC_LIBRARIES :=

# Also need the JNI headers.
LOCAL_C_INCLUDES += \
	$(JNI_H_INCLUDE) \
	$(CUST_H_INCLUDE)

# No specia compiler flags.
LOCAL_CFLAGS += 

# Don't prelink this library.  For more efficient code, you may want
# to add this library to the prelink map and set this to true.
LOCAL_PRELINK_MODULE := false

include $(BUILD_SHARED_LIBRARY)