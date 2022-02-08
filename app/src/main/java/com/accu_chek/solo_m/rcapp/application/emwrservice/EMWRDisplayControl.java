/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRDisplayControl
 * Brief: 
 *
 * Create Date: 2015/6/18
 * $Revision: 20513 $
 * $Author: DWYang $
 * $Id: EMWRDisplayControl.java 20513 2015-10-01 10:25:24Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.emwrservice;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.EMWRConstants;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import android.app.Activity;
import android.app.Application.ActivityLifecycleCallbacks;
import android.content.Context;
import android.os.Bundle;
import android.util.Log;

public class EMWRDisplayControl implements ActivityLifecycleCallbacks
{
    interface EMWRCheckCondition
    {
        /**
         * The function should be override. The check condition should be written in the function.
         *
         * @param context Context of caller
         * Range: valid Context object
         * Unit: Context
         * Scaling: 1
         * 
         * return None 
         */
        public void check(Context context);
    }
    
    static class EMWRActivity implements EMWRCheckCondition
    {
        /**
         * When the activity is EMWRActivity, no condition should be checked.
         *
         * @param context Context of caller
         * Range: valid Context object
         * Unit: Context
         * Scaling: 1
         * 
         * return None
         */
        @Override
        public void check(Context context)
        {
            // Apply to the coding standard
        }
    }
    
    class GeneralActivity implements EMWRCheckCondition
    {
        /**
         * When the activity does not have specific condition, the function should be called.
         * Here just calls EMWR service to show unconfirmed EMWR messages.
         *
         * @param context Context of caller
         * Range: valid Context object
         * Unit: Context
         * Scaling: 1
         * 
         * return None
         */
        @Override
        public void check(Context context)
        {
            SafetyBoolean allowRepeat = NugenGeneralModel.getSafetyBoolean(context, EMWRConstants.KEY_REPEAT, SafetyBoolean.FALSE);
            boolean normalService = (allowRepeat == SafetyBoolean.TRUE);
            
            if (normalService == true)
            {
                NotifyProxy.showEMWR(context);
            }
            else
            {
                // Apply to the coding standard
            }
        }
    }

    private static final String TAG = "EMWRDisplayControl";
    
    /**
     * Set the flag that is to allow EMWR message repeating. 
     *
     * return None
     */
    public static void setAllowRepeatFlag(Context context, SafetyBoolean enable)
    {
        NugenGeneralModel.setSafetyBoolean(context, EMWRConstants.KEY_REPEAT, enable);
    }
    
    
    /**
     * Call when an activity is created. Do nothing here.
     *
     * @param activity Activity that is created.
     * Range: valid Activity object
     * Unit: Activity
     * Scaling: 1
     * @param savedInstanceState Bundle of the created activity.
     * Range: valid Bundle object
     * Unit: Bundle
     * Scaling: 1
     * 
     * return None
     */
    @Override
    public void onActivityCreated(Activity activity, Bundle savedInstanceState)
    {
        // Apply to the coding standard
    }

    /**
     * Call when an activity is started. Do nothing here.
     *
     * @param activity Activity that is started.
     * Range: valid Activity object
     * Unit: Activity
     * Scaling: 1
     * 
     * return None
     */
    @Override
    public void onActivityStarted(Activity activity)
    {
        // Apply to the coding standard
        
    }

    /**
     * Call when an activity is resumed. The class of checking condition is initialized according to input activity.
     *
     * @param activity Activity that is resumed.
     * Range: valid Activity object
     * Unit: Activity
     * Scaling: 1
     * 
     * return None
     */
    @Override
    public void onActivityResumed(Activity activity)
    {
        Debug.printI(TAG, "[onActivityResumed] enter.");
        try
        {
            Class<?> activityCondition = Class.forName(this.getClass().getName()+"$"+activity.getClass().getSimpleName());
            
            Debug.printI(TAG, activityCondition.getClass().getName());
            
            ((EMWRCheckCondition) activityCondition.newInstance()).check(activity);
        }
        catch (ClassNotFoundException e)
        {
            Debug.printE(TAG, "ClassNotFoundException: " + activity.getClass().getName());
            
            new GeneralActivity().check(activity);
        }
        catch (InstantiationException e)
        {
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
        
    }

    /**
     * Call when an activity is paused. Do nothing here.
     *
     * @param activity Activity that is paused.
     * Range: valid Activity object
     * Unit: Activity
     * Scaling: 1
     * 
     * return None
     */
    @Override
    public void onActivityPaused(Activity activity)
    {
        // Apply to the coding standard
        
    }

    /**
     * Call when an activity is stopped. Do nothing here.
     *
     * @param activity Activity that is stopped.
     * Range: valid Activity object
     * Unit: Activity
     * Scaling: 1
     * 
     * return None
     */
    @Override
    public void onActivityStopped(Activity activity)
    {
        // Apply to the coding standard
        
    }

    /**
     * Call when an activity is saved instance state. Do nothing here.
     *
     * @param activity Activity that is saved instance state.
     * Range: valid Activity object
     * Unit: Activity
     * Scaling: 1
     * @param outState Bundle of the activity.
     * Range: valid Bundle object
     * Unit: Bundle
     * Scaling: 1
     * 
     * return None
     */
    @Override
    public void onActivitySaveInstanceState(Activity activity, Bundle outState)
    {
        // Apply to the coding standard
        
    }

    /**
     * Call when an activity is destroyed. Do nothing here.
     *
     * @param activity Activity that is destroyed.
     * Range: valid Activity object
     * Unit: Activity
     * Scaling: 1
     * 
     * return None
     */
    @Override
    public void onActivityDestroyed(Activity activity)
    {
        // Apply to the coding standard
    }

}
