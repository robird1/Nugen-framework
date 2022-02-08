/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: UpdateConfig
 * Brief: This class applies AIDL mechanism to update system language by the
 * RCSystemService process.
 * 
 * Create Date: 08/20/2015
 * $Revision: 21156 $
 * $Author: SteveSu $
 * $Id: UpdatConfig.java 21156 2015-10-07 09:02:40Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.application.setting.generalsetting;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Locale;

import android.app.Activity;
import android.content.Context;
import android.content.res.Configuration;
import android.os.RemoteException;
import android.view.View;
import android.view.Window;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.SettingConstants;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class UpdateConfig extends IPCUpdateConfig.Stub 
{

    // This tag is for debugging
    private static final String TAG = "UpdateSystemLanguage";
	
	// Number of enum constant of Language
    private static final int TOTAL_LANGUAGE_NUMBER = Language.values().length;
	
	// The context of RCSystemService
    private Context mContext = null;
	
	// For updating system language
    private Locale[] mLanguages = new Locale[TOTAL_LANGUAGE_NUMBER];
    
    // For storing the selected language
    private String[] mLanguageCode = new String[TOTAL_LANGUAGE_NUMBER];
    
    /**
     * This class constructor shall initialize the class attributes.
     * 
     * @param context : The context of RCSystemService.
     *            Range: valid object 
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mContext
     * @see mLanguages
     * @see mLanguageCode
     * 
     * @throw RemoteException
     */
    public UpdateConfig(final Context context)
    {
        int i = 0;

        mContext = context;
		
        for (Language lan: Language.values())
        {
            mLanguages[i] = new Locale(lan.getLanguageCode());
		    
            mLanguageCode[i++] = lan.getLanguageCode();
        }
    }
	
    /**
     * Update system language.
     * 
     * @param languageIndex : The enum constant index of Language.
     *            Range: 0 ~ 30 
     *            Unit: int
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mLanguages
     * @see mContext
     * 
     * @throw RemoteException
     */
    @Override
	public void updateLanguage(final int languageIndex) throws RemoteException
    {
        storeSelectedLanguage(languageIndex);
        
        try 
        {
            Window window = null;
            Class<?> amnClass = Class.forName("android.app.ActivityManagerNative");
            Object amn = null;
            Configuration config = null;
            Method methodGetConfiguration = null;
            Class<?> configClass = null;
            Field f = null;
            Method methodUpdateConfiguration = null;
			
			// amn = ActivityManagerNative.getDefault();
            final Method methodGetDefault = amnClass.getMethod("getDefault");
            methodGetDefault.setAccessible(true);
            amn = methodGetDefault.invoke(amnClass);

			// config = amn.getConfiguration();
            methodGetConfiguration = amnClass.getMethod("getConfiguration");
            methodGetConfiguration.setAccessible(true);
            config = (Configuration) methodGetConfiguration.invoke(amn);

			// config.userSetLocale = true;
            configClass = config.getClass();
            f = configClass.getField("userSetLocale");
            f.setBoolean(config, true);

			// set the locale to the new value
            config.locale = mLanguages[languageIndex];

			// amn.updateConfiguration(config);
            methodUpdateConfiguration = amnClass.getMethod("updateConfiguration", Configuration.class);
            methodUpdateConfiguration.setAccessible(true);
            methodUpdateConfiguration.invoke(amn, config);
			
            window = ((Activity) mContext).getWindow();
            if (window != null)
            {
                window.getDecorView().setLayoutDirection(View.LAYOUT_DIRECTION_LOCALE);
            }
			
        } 
        catch (ClassNotFoundException e) 
        {
            e.printStackTrace();
            Debug.printI(TAG, "ClassNotFoundException...");

        } 
        catch (NoSuchMethodException e) 
        {
            e.printStackTrace();
            Debug.printI(TAG, "NoSuchMethodException...");

        } 
        catch (IllegalAccessException e) 
        {
            e.printStackTrace();
            Debug.printI(TAG, "IllegalAccessException...");

        } 
        catch (InvocationTargetException e) 
        {
            e.printStackTrace();
            Debug.printI(TAG, "InvocationTargetException...");

        } 
        catch (NoSuchFieldException e) 
        {
            e.printStackTrace();
            Debug.printI(TAG, "NoSuchFieldException...");

        }
        finally
        {
		    // do nothing.
        }

    }

    /**
     * Store the selected language in share preference.
     * 
     * @param index : The enum constant index of Language.
     *            Range: 0 ~ 30 
     *            Unit: int
     *            Scaling: 1
     * 
     * @return None
     * 
     * @see mLanguageCode
     * @see mContext
     */
    private void storeSelectedLanguage(final int index)
    {
        final SafetyString sLanguage = new SafetyString();
        sLanguage.set(mLanguageCode[index], CRCTool.generateCRC16(mLanguageCode[index].getBytes()));
        
        Debug.printI(TAG, "language code: " + mLanguageCode[index]);

        NugenSettingModel.setString(mContext, SettingConstants.LANGUAGE, sLanguage);
    }

    public enum Language
    {
        selectableLanguage01(0x0001,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_01, "en-US",
            R.string.txt_languageenglish),
        selectableLanguage02(0x0002,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_02, "de",
            R.string.txt_languagegerman),
        selectableLanguage03(0x0003,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_03, "el",
            R.string.txt_languagegreek),
        selectableLanguage04(0x0004,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_04, "it",
            R.string.txt_languageitalian),
        selectableLanguage05(0x0005,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_05, "zh-Hans",
            R.string.txt_languagezhs),
        selectableLanguage06(0x0006,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_06, "zh-Hant",
            R.string.txt_languagezht),
        selectableLanguage07(0x0007,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_07, "fr",
            R.string.txt_languagefrench),
        selectableLanguage08(0x0008,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_08, "pl",
            R.string.txt_languagepolski),
        selectableLanguage09(0x0009,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_09, "en",
            R.string.txt_languageenglish),
        selectableLanguage10(0x000A,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_10, "es",
            R.string.txt_languagespanish),
        selectableLanguage11(0x000B,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_11, "pt-BR",
            R.string.txt_languageportugues),
        selectableLanguage12(0x000C,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_12, "pt-PT",
            R.string.txt_languageportugues),
        selectableLanguage13(0x000D,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_13, "sv",
            R.string.txt_languagesvenska),
        selectableLanguage14(0x000E,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_14, "da",
            R.string.txt_languagedansk),
        selectableLanguage15(0x000F,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_15, "he",
            R.string.txt_languagehebrew),
        selectableLanguage16(0x0010,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_16, "no",
            R.string.txt_languagenorsk),
        selectableLanguage17(0x0011,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_17, "sk",
            R.string.txt_languagesrpski),                                // TODO : lack string resource
        selectableLanguage18(0x0012,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_18, "fi",
            R.string.txt_languagefinnish),
        selectableLanguage19(0x0013,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_19, "sl",
            R.string.txt_languageslovencina),
        selectableLanguage20(0x0014,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_20, "cs",
            R.string.txt_languagecesky),
        selectableLanguage21(0x0015,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_21, "hu",
            R.string.txt_languagehebrew),                               // TODO : lack string resource
        selectableLanguage22(0x0016,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_22, "nl",
            R.string.txt_languagenetherlands),
        selectableLanguage23(0x0017,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_23, "ru",
            R.string.txt_languagerussia),
        selectableLanguage24(0x0018,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_24, "ja-Hira",
            R.string.txt_languagejapan),
        selectableLanguage25(0x0019,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_25, "ja-Kana",
            R.string.txt_languagejapan),
        selectableLanguage26(0x001A,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_26, "ar",
            R.string.txt_languagearabic),
        selectableLanguage27(0x001B,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_27, "kr",
            R.string.txt_languagekorean),
        selectableLanguage28(0x001C,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_28, "fr-can",
            R.string.txt_languagefrench),
        selectableLanguage29(0x001D,
            ProductionConstants.KEY_SELECTABLE_LANGUAGE_29, "tr",
            R.string.txt_languageturkey);
//        selectableLanguage30(0x001E,
//            ProductionConstants.KEY_SELECTABLE_LANGUAGE_30, "hr",
//            R.string.txt_languagehrvatski),                                 
//        selectableLanguage31(0x001F,
//            ProductionConstants.KEY_SELECTABLE_LANGUAGE_31, "sr",
//            R.string.txt_languagesrpski);

        /**
         * Language identifier. The default value can only be assigned once and
         * it will be assigned in constructor.
         */
        private final int mLanguageID;

        /**
         * The key of language to access production model. The default value can
         * only be assigned once and it will be assigned in constructor.
         */
        private final String mKey;

        /**
         * The key for accessing selected Language by Setting Interface. The
         * default value can only be assigned once and it will be assigned in
         * constructor.
         */
        private final String mLanguageCode;

        /**
         * Text resource ID of certain language. The default value can only be
         * assigned once and it will be assigned in constructor.
         */
        private final int mTextId;

        /**
         * This enum constructor shall initialize the attributes of the enum constant.
         * 
         * @param languageID : The language identifier defined in Configuration Matrix.
         *            Range: 1, 2 , 3, ..., 30, 31
         *            Unit: int
         *            Scaling: 1
         * @param productionKey : The key to access language in production model.
         *            Range: "selectableLanguage01", "selectableLanguage02", ... , "selectableLanguage30", "selectableLanguage31"
         *            Unit: String
         *            Scaling: 1
         * @param languageCode : The key for accessing selected Language by Setting Interface
         *            Range: en-US, de, el, it, zh-Hans, zh-Hant, fr, pl, en, es,
         *                   pt-BR, pt-PT, sv, da, he, no, sk, fi, sl, cs, hu, nl, ru,
         *                   ja-Hira, ja-Kana, ar, kr, fr-can, tr
         *            Unit: String
         *            Scaling: 1
         * @param textID : Language text resource ID.
         *            Range: -2^31 to (2^31)-1
         *            Unit: int
         *            Scaling: 1
         *            
         * @return None
         * 
         * @see mLanguageID
         * @see mKey
         * @see mLanguageCode
         * @see mTextId
         */
        private Language(final int languageID, final String productionKey, final String languageCode, final int textID)
        {
            mLanguageID = languageID;
            mKey = productionKey;
            mLanguageCode = languageCode;
            mTextId = textID;
        }

        /**
         * Get language identifier of a certain enum constant.
         * 
         * @return Language identifier of a certain enum constant.
         *         Range: 1, 2, 3, ... , 30, 31
         *         Unit: int
         *         Scaling: 1
         * 
         * @see mLanguageID
         */
        public int getLanguageID()
        {
            return mLanguageID;
        }

        /**
         * Get text resource ID of a certain enum constant.
         * 
         * @return Text resource ID of a certain enum constant.
         *         Range: -2^31 to (2^31)-1
         *         Unit: int
         *         Scaling: 1
         * 
         * @see mTextId
         */
        public int getResource()
        {
            return mTextId;
        }

        /**
         * Return the production model key of this enum constant.
         * 
         * @return The key of this enum constant.
         *         <p>
         *         Range: "selectableLanguage01", "selectableLanguage02", ... ,
         *         "selectableLanguage30", "selectableLanguage31"
         *         <p>
         *         Unit: String 
         *         Scaling: 1
         * 
         * @see mKey
         */
        public String getKey()
        {
            return mKey;
        }

        /**
         * Return the production model key of this enum constant.
         * 
         * @return The production model key of this enum constant.
         *         <p>
         *         Range: en-US, de, el, it, zh-Hans, zh-Hant, fr, pl, en, es,
         *         pt-BR, pt-PT, sv, da, he, no, sk, fi, sl, cs, hu, nl, ru,
         *         ja-Hira, ja-Kana, ar, kr, fr-can, tr
         *         <p>
         *         Unit: String 
         *         Scaling: 1
         * 
         * @see mLanguageCode
         */
        public String getLanguageCode()
        {
            return mLanguageCode;
        }

        /**
         * A util function for obtaining a certain enum constant from a given
         * language code.
         * 
         * @param code : The code which corresponds to the language type.
         *            <p>
         *            Range: en-US, de, el, it, zh-Hans, zh-Hant, fr, pl, en,
         *            es, pt-BR, pt-PT, sv, da, he, no, sk, fi, sl, cs, hu, nl,
         *            ru, ja-Hira, ja-Kana, ar, kr, fr-can, tr
         *            <p>
         *            Unit: String Scaling: 1
         * 
         * @return An enum constant.
         *         Range: enum constant of Language
         *         Unit: Language
         *         Scaling: 1
         */
        public static Language fromLanguageCode(final String code)
        {
            // default language: English
            Language type = Language.selectableLanguage09;

            for (Language t : Language.values())
            {
                if (t.getLanguageCode().equals(code))
                {
                    type = t;
                }
            }

            return type;
        }

        /**
         * A util function for obtaining a certain enum constant from a given text resource
         * ID.
         * 
         * @param textId : The text resource ID which corresponds to the language type.
         *            Range: -2^31 to (2^31)-1
         *            Unit: int
         *            Scaling: 1
         * 
         * @return An enum constant.
         *         Range: enum constant of Language
         *         Unit: Language
         *         Scaling: 1
         */
        public static Language fromResourceId(final int textId)
        {
            // default language: English
            Language type = Language.selectableLanguage09;

            for (Language t : Language.values())
            {
                final int temp = t.getResource();

                if (temp == textId)
                {
                    type = t;
                }
            }

            return type;
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
// [Setting] general setting
//----------------------------------------------------------------------------
// [Setting] add missing file
// (R15209 2015-08-22 23:31:51 henrytso)
// ----------------------------------------------------------------------------
// [Fixed NSIQ-77]
// (R16158 2015-09-02 06:11:11 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] add function comment
// (R17028 2015-09-10 23:10:50 SteveSu)
// ----------------------------------------------------------------------------
// [Fixed NSIQ-150]
// (R20551 2015-10-01 09:43:53 DWYang)
// ----------------------------------------------------------------------------
// [Setting] 1. fix Klocwork issues
// 2. fix Checkstyle issues
// (R21079 2015-10-06 05:50:12 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] 1. add function comment
// 2. function renaming
// (R21156 2015-10-07 05:02:40 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] rename class name
// (R21156 2015-10-07 05:02:40 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] Add and update comment
// (R21156 2015-10-07 05:02:40 SteveSu)
// ----------------------------------------------------------------------------
// [Settings] update code based on code review findings

// (R21156 2015-10-07 05:02:40 SteveSu)
// ----------------------------------------------------------------------------
// [Setting] update Hamming values defined in CM V8
