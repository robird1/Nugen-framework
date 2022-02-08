/** //device/java/android/android/hardware/IJavaFrameworkService.aidl
* This is java framework service interface definition
* This is template file.
*/

package com.accu_chek.solo_m.rcapp.application.power;



/**
 * {@hide}
 */
interface ICustPowerManager
{
    // shutdown function
    void shutdown();
    
    // reboot function
    void reboot();
    
    // acquire wake lock
    void newWakeLock();
    
    // release wake lock
    void release();
    
    // return pump battery status
    String getPumpBatteryStatus();
}
