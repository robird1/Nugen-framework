/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: NotificationCenter
 * Brief: Provide the NotificationCenter function
 *
 * Create Date: 12/09/2014
 * $Revision: 24958 $
 * $Author: AdamChen $
 * $Id: NotificationCenter.java 24958 2015-11-27 01:02:55Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.content.Context;
import android.support.v4.app.NotificationManagerCompat;


public class NotificationCenter
{

public static final int KEY_BOLUS = 0;
    
    public static final int KEY_MESSAGE = 1;
    
    public static final int KEY_TBR = 2;
    
    public static final int KEY_SLB = 3;
    
    public static final int KEY_TOTAL = 4;
    
    public static final int KEY_ERROR = 5;
    
    final static String GROUP_KEY_EMAILS = "group_key_emails";
    NotificationManagerCompat notificationManager;
        
    private static NotificationCenter instance;
    
    private Context mCtx = null;
    
    private int noti_counter = 0; // Notification counter !!!Important!!! Reset
                                  // to 0
    
    /**
     * Restrict the constructor from being instantiated
     */
    private NotificationCenter()
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
        notificationManager = NotificationManagerCompat.from(mCtx);
        notificationManager.cancelAll();
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return NotificationCenter [out] Delete pre line return if exist. Parameter Description
     */
    public static synchronized NotificationCenter getInstance()
    {
        if (instance == null)
        {
            instance = new NotificationCenter();
        }
        return instance;
    }

    /**
     * 
     * Function Description
     *
     * @param Ticker
     * @param Text1
     * @param Text2
     * @param Text3
     * @param image
     * @param key
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void create_B29_notify(String Ticker, String Text1, String Text2,
            String Text3, int image, int key)
    {
        Button_B29 b29 = new Button_B29(mCtx);
        Button_B29.nImage = image;
        Button_B29.nText1 = Text1;
        Button_B29.nText2 = Text2;
        Button_B29.nText3 = Text3;
        Button_B29.nTicker = Ticker;
        b29.sendNotify(key);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */