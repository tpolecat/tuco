package tuco.util;

import net.wimpi.telnetd.shell.Shell;

import java.util.Map;
import java.util.Properties;
import java.util.function.Supplier;

public interface JavaConfig {
  Properties properties();
  Map<String, Supplier<Shell>> factories();
}
