/** //device/java/android/android/hardware/IJavaFrameworkService.aidl
* This is java framework service interface definition
* This is templete file.
*/

package com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;



/**
 * {@hide}
 */
interface IUICommandDispatcher
{
    //export function
    int submitRequest(in SafetyByteArray byteArr);
    void resetComms();
}
