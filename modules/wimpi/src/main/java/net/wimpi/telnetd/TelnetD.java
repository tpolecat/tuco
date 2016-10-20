//License
/***
 * Java TelnetD library (embeddable telnet daemon)
 * Copyright (c) 2000-2005 Dieter Wimberger
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS
 * IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ***/

package net.wimpi.telnetd;

import net.wimpi.telnetd.io.terminal.TerminalManager;
import net.wimpi.telnetd.net.PortListener;
import net.wimpi.telnetd.shell.ShellManager;
import net.wimpi.telnetd.util.PropertiesLoader;
import net.wimpi.telnetd.util.StringUtil;
import tuco.util.JavaConfig;

import java.util.logging.Logger;


import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * Class that implements a configurable and embeddable
 * telnet daemon.
 *
 * @author Dieter Wimberger
 * @version 2.0 (16/07/2006)
 */
public class TelnetD {

  private static final Logger log = Logger.getLogger(TelnetD.class.getName());

  private final List<PortListener> listeners = new ArrayList<>(5);
  private final ShellManager shellManager;
  private final TerminalManager terminalManager;

  public TelnetD(ShellManager sm, TerminalManager tm) {
    shellManager = sm;
    terminalManager = tm;
  }

  /**
   * Start this telnet daemon, respectively
   * all configured listeners.<br>
   */
  public void start() {
    log.fine("start()");
    listeners.forEach(PortListener::start);
  }//start

  /**
   * Stop this telnet daemon, respectively
   * all configured listeners.
   */
  public void stop() {
    listeners.forEach(PortListener::stop);
  }//stop

  /**
   * Method to prepare the PortListener.<br>
   * Creates and prepares and runs a PortListener, with settings from the
   * passed in Properties. Yet the Listener will not accept any incoming
   * connections before startServing() has been called. this has the advantage
   * that whenever a TelnetD Singleton has been factorized, it WILL 99% not fail
   * any longer (e.g. serve its purpose).
   *
   * @param settings Properties object that holds main settings.
   * @throws BootException if preparation fails.
   */
  public void prepareListener(String name, Properties settings)
      throws BootException {

    //factorize PortListener
    PortListener listener = PortListener.createPortListener(name, settings, shellManager, terminalManager);
    //start the Thread derived PortListener
    try {
      listeners.add(listener);
    } catch (Exception ex) {
      throw new BootException("Failure while starting PortListener thread: " + ex.getMessage());
    }

  }//prepareListener


  /**
   * Returns a {@link PortListener} for the given
   * identifier.
   *
   * @param id the identifier of the {@link PortListener} instance.
   * @return {@link PortListener} instance or null if an instance
   *         with the given identifier does not exist.
   */
  public PortListener getPortListener(String id) {
    if(id==null || id.length() == 0) {
      return null;
    }
    for (PortListener portListener : listeners) {
      if (portListener.getName().equals(id)) {
        return portListener;
      }
    }
    return null;
  }//getPortListener

  /**
   * Factory method to create a TelnetD Instance.
   *
   * @param main Properties object with settings for the TelnetD.
   * @return TenetD instance that has been properly set up according to the
   *         passed in properties, and is ready to start serving.
   * @throws BootException if the setup process fails.
   */
  public static TelnetD createTelnetD(JavaConfig config)  throws BootException {
      Properties main = config.properties();
      TelnetD td = new TelnetD(new ShellManager(config.factories()), TerminalManager.createTerminalManager(main));
      String[] listnames = StringUtil.split(main.getProperty("listeners"), ",");
    for (String listname : listnames) {
      td.prepareListener(listname, main);
    }
      return td;
  }//createTelnetD

}//class TelnetD
