/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B29
 * Brief: Provide the function of the Button_B29 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24958 $
 * $Author: AdamChen $
 * $Id: Button_B29.java 24958 2015-11-27 01:02:55Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Notification;
import android.app.NotificationManager;
import android.content.Context;
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.NotificationCompat.Builder;
import android.widget.RemoteViews;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class Button_B29
{

    public static String nTicker = "";
    
    public static String nText1 = "";
    
    public static String nText2 = "";
    
    public static String nText3 = "";
    
    public static int nImage = 0;
    
    private Context ctx;

    /**
     * 
     * Function Description
     *
     * @param key
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void sendNotify(int key)
    {
        Notification foregroundNote;
        RemoteViews small = new RemoteViews(ctx.getApplicationContext()
                .getPackageName(), R.layout.button_b29);

        Builder mNotifyBuilder = new NotificationCompat.Builder(ctx);
        foregroundNote = mNotifyBuilder.setContentTitle(nText1)
                .setContentText(nText2).setSmallIcon(nImage).setContent(small)
                .setSubText(nText3).setOngoing(true)
                .setPriority(Notification.PRIORITY_MIN).build();

        small.setImageViewResource(R.id.id_b29_image, nImage);

        small.setTextViewText(R.id.id_b29_text, nText1);
        small.setTextViewText(R.id.id_b29_text2, nText2);
        small.setTextViewText(R.id.id_b29_text3, nText3);

        // now show notification..
        NotificationManager mNotifyManager = (NotificationManager) ctx
                .getSystemService(Context.NOTIFICATION_SERVICE);
        mNotifyManager.notify(key, foregroundNote);

    }

    /**
     * 
     * @param c
     */
    public Button_B29(Context c)
    {
        ctx = c;
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
