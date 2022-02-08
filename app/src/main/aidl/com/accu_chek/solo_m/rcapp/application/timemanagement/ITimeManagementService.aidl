
package com.accu_chek.solo_m.rcapp.application.timemanagement;

import com.accu_chek.solo_m.rcapp.application.TimeManagement.IDateTimeSettingCallback;
/**
 * {@hide}
 */
interface ITimeManagementService
{
    //export function
    void backupRCTime();
    void restoreRCTime();
    void setRCTime(in long timestamp);
    long getRCTime();
    void resetBGMTime();
    void syncBGMTime();
    boolean verifyBGMTime();
    byte[] verifyPumpTime(in byte[] pumpdata);
    void registerTimeSettingCallback(IDateTimeSettingCallback cb);
    void unregisterTimeSettingCallback(IDateTimeSettingCallback cb);
}