/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.cgmapp.application.safety.ISafetyEquals
 * Brief: The interface that defined the safetyEquals function.
 *
 * Create Date: 1/6/2014
 * $Revision: 24182 $
 * $Author: StanleySu $
 * $Id: ISafetyEquals.java 24182 2015-11-16 09:41:33Z StanleySu $
 */

package com.accu_chek.solo_m.rcapp.application.safety;

interface ISafetyEquals<T, D>
{
    
    /**
     * Return if the original data is equals diverse data. Otherwise return
     * false.
     * 
     * @param original [in] the original data
     * @param diverse [in] the diverse data
     * @return SafetyBoolean [out] return true if the original data is equals
     *         diverse data. Otherwise return false.
     */
    public SafetyBoolean safetyEquals(T original, D diverse);
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// add comments
// (3338 2014-01-06 06:57:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// Restore the sources to revision 3682.
// (3700 2014-01-22 08:38:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Undone KEYWORD function of SVN. No Source code content will be
// changed
// (3700 2014-01-22 08:38:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Preform KEYWORD function of SVN for each source files. No Source
// code content will be changed
// (21021 2014-10-03 02:34:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
