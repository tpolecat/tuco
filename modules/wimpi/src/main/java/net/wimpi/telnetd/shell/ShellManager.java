package net.wimpi.telnetd.shell;

import java.util.Map;
import java.util.function.Supplier;

public class ShellManager {

  private final Map<String, Supplier<Shell>> factories;

  public ShellManager(Map<String, Supplier<Shell>> factories) {
    this.factories = factories;
  }

  public Shell getShell(String key) {
    Supplier<Shell> s = factories.get(key);
    return (s == null) ? null : s.get();
  }

}
