/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: GlobalContainer
 * Brief: This class provide the common tool for all module in RC app
 * 
 * Create Date: 05/27/2015
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: GlobalContainer.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.globaltools;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.os.Handler;
import android.os.Message;
import android.text.format.Time;
import android.util.DisplayMetrics;
import android.widget.ImageView;

import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

//this class is only for storing data used globally in app
public class GlobalContainer
{

    private static final int TIMER_MSG_CONTENT = 999;

    public static String BGMeasuredResult = "84";
    
    public static int configLanguage = R.string.txt_languagegerman;
      
    public static ImageView mBatteryView = null;
    
    private static GlobalContainer mInstance = null;
    
    private Context mCtx = null;

    private SharedPreferences sharedPref;
    
    private SharedPreferences.Editor editor;
    
    private String language = "de";
    
    public String timeformatted = "";

    int initialDelay = 60000; // first update in miliseconds
    
    int period = 60000; // nexts updates in miliseconds
    
    Handler mTime = null;
   
    Time time = null;
    
    Timer timer = null;
    
    TimerTask task = null;

    
    /**
     * 
     * Function Description
     *
     * @param time
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void registerTimeHandler(Handler time)
    {
        mTime = time;
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void stopTimerTask()
    {
        mTime = null;
        task.cancel();
        timer.cancel();

    }

    /**
     * Restrict the constructor from being instantiated
     */
    private GlobalContainer()
    {
        
    }

    /**
     * 
     * Function Description
     *
     * @param context
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void init(Context context)
    {
        
        this.mCtx = context;
        time = new Time();
        sharedPref = context.getSharedPreferences("com.accu_chek.solom",
                Context.MODE_PRIVATE);
        editor = sharedPref.edit();

        time.setToNow();
        timeformatted = time.format("%H:%M");
        timer = new Timer();
        task = new TimerTask()
        {
            public void run()
            {
                time.setToNow();
                timeformatted = time.format("%H:%M");
                
                    Message msg = Message.obtain();
                msg.what = TIMER_MSG_CONTENT;
                    mTime.sendMessage(msg);
                    
                }
        };

        timer.scheduleAtFixedRate(task, initialDelay, period);
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return GlobalContainer [out] Delete pre line return if exist. Parameter Description
     */
    public static synchronized GlobalContainer getInstance()
    {
        if (mInstance == null)
        {
            mInstance = new GlobalContainer();
        }
        return mInstance;
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void refreshLanguage()
    {
        setLanguage(this.language);
    }

    /**
     * 
     * Function Description
     *
     * @param lang
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLanguage(String lang)
    {
        Resources res = mCtx.getResources();
        language = lang;
        DisplayMetrics dm = res.getDisplayMetrics();
        android.content.res.Configuration conf = res.getConfiguration();
        conf.locale = new Locale(language.toLowerCase());
        Locale.setDefault(conf.locale);
        res.updateConfiguration(conf, dm);
    }

    /**
     * 
     * Function Description
     *
     * @param key
     * @param Value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void store(String key, String Value)
    {
        editor.putString(key, Value);
        editor.commit();
    }

    /**
     * 
     * Function Description
     *
     * @param key
     * @param Value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void store(String key, int Value)
    {
        editor.putInt(key, Value);
        editor.commit();
    }

    /**
     * 
     * Function Description
     *
     * @param key
     * @param Value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void store(String key, Boolean Value)
    {
        editor.putBoolean(key, Value);
        editor.commit();
    }

    // add by Steve {
    /**
     * 
     * Function Description
     *
     * @param key
     * @param Value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void store(String key, long Value)
    {
        editor.putLong(key, Value);
        editor.commit();
    }
    // } add by Steve

    /**
     * 
     * Function Description
     *
     * @param key
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int restoreInt(String key)
    {
        return sharedPref.getInt(key, 0);
    }

    /**
     * 
     * Function Description
     *
     * @param key
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public String restoreString(String key)
    {
        return sharedPref.getString(key, null);
    }

    /**
     * 
     * Function Description
     *
     * @param key
     * @return
     * @return Boolean [out] Delete pre line return if exist. Parameter Description
     */
    public Boolean restoreBoolean(String key)
    {
        return sharedPref.getBoolean(key, false);
    }

    // add by Steve {
    /**
     * 
     * Function Description
     *
     * @param key
     * @return
     * @return long [out] Delete pre line return if exist. Parameter Description
     */
    public long restoreLong(String key)
    {
        return sharedPref.getLong(key, 0);
    }
    // } add by Steve

    /**
     * 
     */
    public final class STORE_KEY
    {
        public static final String SETUP_COMPLETE = "SETUP_COMPLETE";
        public static final String SOUND = "SOUND";

        private STORE_KEY()
        {
        }
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [GUI] update GUI framework to ClickThrough v0.34
