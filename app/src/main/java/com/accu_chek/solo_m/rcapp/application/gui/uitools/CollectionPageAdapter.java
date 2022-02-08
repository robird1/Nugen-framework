/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: CollectionPageAdapter
 * Brief: Provide the CollectionPageAdapter function
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: CollectionPageAdapter.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.view.ViewPager;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.FragmentChangeListener;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class CollectionPageAdapter extends FragmentStatePagerAdapter
{

    Fragment fragment1 = null;
    
    Fragment fragment2 = null;
    
    ViewPager mViewPager = null;
    
    ViewGroup mContainer = null;
    
    FragmentChangeListener mChange = null;
    
    int mPos = 0;

    /**
     * 
     * @param fm
     * @param container
     * @param f1
     * @param f2
     */
    public CollectionPageAdapter(
            FragmentManager fm, ViewGroup container, Fragment f1, Fragment f2)
    {
        super(fm);
        fragment1 = f1;
        fragment2 = f2;
        mContainer = container;

        mViewPager = (ViewPager) container.findViewById(R.id.pager);
        mViewPager.setAdapter(this);
        mViewPager
                .setOnPageChangeListener(new ViewPager.SimpleOnPageChangeListener()
                {
                    @Override
                    public void onPageSelected(int position)
                    {
                        mPos = position;
                        // When swiping between pages, select the
                        // corresponding tab.
                        if (mChange != null)
                        {
                            mChange.onFragmentChanged(position);
                        }
                        
                        if (position == 0)
                        {
                            activateLeftBar(true);
                        }
                        else
                        {
                            activateLeftBar(false);
                        }
                    }
                });
    }

    /**
     * 
     * @param fm
     * @param container
     * @param f1
     */
    public CollectionPageAdapter(
            FragmentManager fm, ViewGroup container, Fragment f1)
    {
        super(fm);
        fragment1 = f1;
        mContainer = container;
        mViewPager = (ViewPager) container.findViewById(R.id.pager);
        mViewPager.setAdapter(this);
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setChangeListener()
    {

    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getPos()
    {
        return mPos;
    }

    /**
     * 
     * 
     *
     * @param i
     * @return
     */
    @Override
    public Fragment getItem(int i)
    {
        Fragment ret = null;
        
        switch (i)
        {
        case 0:
            ret = fragment1;
            break;
        case 1:
            ret = fragment2;
            break;
        default:
            break;
        }
        return ret;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getCount()
    {
        int ret = 2;
        
        if (fragment2 == null)
        {
            ret = 1;
        }
        
        return ret;
    }

    /**
     * 
     * Function Description
     *
     * @param activate_left
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void activateLeftBar(Boolean activate_left)
    {
        RelativeLayout greenbar_left = (RelativeLayout) mContainer
                .findViewById(R.id.id_tab2_green_left);
        RelativeLayout greenbar_right = (RelativeLayout) mContainer
                .findViewById(R.id.id_tab2_green_right);
        if (activate_left)
        {

            greenbar_left.setBackgroundResource(R.color.insulin_button);
            greenbar_right.setBackgroundResource(0);
        }
        else
        {

            greenbar_right.setBackgroundResource(R.color.insulin_button);
            greenbar_left.setBackgroundResource(0);

        }
    }

    /**
     * 
     * Function Description
     *
     * @param left_active
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setLeftActive(boolean left_active)
    {
        if (left_active)
        {
            mViewPager.setCurrentItem(0);
            activateLeftBar(true);
        }
        else
        {
            mViewPager.setCurrentItem(1);
            activateLeftBar(false);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param change
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setChangeListener(FragmentChangeListener change)
    {
        mChange = change;

    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */

