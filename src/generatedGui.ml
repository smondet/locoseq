(* Automatically generated from gui_files/app.glade by lablgladecc *)

let data = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n<!DOCTYPE glade-interface SYSTEM \"glade-2.0.dtd\">\n<!--*- mode: xml -*-->\n<glade-interface>\n  <widget class=\"GtkWindow\" id=\"app_window\">\n    <property name=\"visible\">True</property>\n    <property name=\"has_focus\">True</property>\n    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK | GDK_KEY_PRESS_MASK</property>\n    <property name=\"title\" translatable=\"yes\">not set</property>\n    <property name=\"default_width\">780</property>\n    <property name=\"default_height\">500</property>\n    <child>\n      <widget class=\"GtkHBox\" id=\"hbox6\">\n        <property name=\"visible\">True</property>\n        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n        <child>\n          <widget class=\"GtkVBox\" id=\"vbox7\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label_title\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">\n&lt;b&gt;not set&lt;/b&gt;</property>\n                <property name=\"use_markup\">True</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkFrame\" id=\"frame1\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label_xalign\">0</property>\n                <property name=\"shadow_type\">GTK_SHADOW_IN</property>\n                <child>\n                  <widget class=\"GtkAlignment\" id=\"alignment3\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"left_padding\">12</property>\n                    <child>\n                      <widget class=\"GtkHBox\" id=\"hbox7\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <property name=\"homogeneous\">True</property>\n                        <child>\n                          <widget class=\"GtkTable\" id=\"table3\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"n_rows\">3</property>\n                            <property name=\"n_columns\">2</property>\n                            <child>\n                              <widget class=\"GtkButton\" id=\"button_new\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"receives_default\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">New</property>\n                              </widget>\n                            </child>\n                            <child>\n                              <widget class=\"GtkButton\" id=\"button_open\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"receives_default\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">Open</property>\n                              </widget>\n                              <packing>\n                                <property name=\"left_attach\">1</property>\n                                <property name=\"right_attach\">2</property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkButton\" id=\"button_save\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"receives_default\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">Save</property>\n                              </widget>\n                              <packing>\n                                <property name=\"top_attach\">1</property>\n                                <property name=\"bottom_attach\">2</property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkButton\" id=\"button_saveas\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"receives_default\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">Save As</property>\n                              </widget>\n                              <packing>\n                                <property name=\"left_attach\">1</property>\n                                <property name=\"right_attach\">2</property>\n                                <property name=\"top_attach\">1</property>\n                                <property name=\"bottom_attach\">2</property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkButton\" id=\"button_quit\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"receives_default\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">Quit</property>\n                              </widget>\n                              <packing>\n                                <property name=\"top_attach\">2</property>\n                                <property name=\"bottom_attach\">3</property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkButton\" id=\"button_help\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"receives_default\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">Help</property>\n                              </widget>\n                              <packing>\n                                <property name=\"left_attach\">1</property>\n                                <property name=\"right_attach\">2</property>\n                                <property name=\"top_attach\">2</property>\n                                <property name=\"bottom_attach\">3</property>\n                              </packing>\n                            </child>\n                          </widget>\n                        </child>\n                        <child>\n                          <widget class=\"GtkVBox\" id=\"vbox8\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <child>\n                              <widget class=\"GtkButton\" id=\"button_play\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"receives_default\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">Play</property>\n                              </widget>\n                            </child>\n                            <child>\n                              <widget class=\"GtkButton\" id=\"button_stop\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"receives_default\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">Stop</property>\n                              </widget>\n                              <packing>\n                                <property name=\"position\">1</property>\n                              </packing>\n                            </child>\n                          </widget>\n                          <packing>\n                            <property name=\"position\">1</property>\n                          </packing>\n                        </child>\n                      </widget>\n                    </child>\n                  </widget>\n                </child>\n                <child>\n                  <widget class=\"GtkLabel\" id=\"label6\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"label\" translatable=\"yes\">&lt;b&gt;Buttons:&lt;/b&gt;</property>\n                    <property name=\"use_markup\">True</property>\n                  </widget>\n                  <packing>\n                    <property name=\"type\">label_item</property>\n                  </packing>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkFrame\" id=\"frame2\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label_xalign\">0</property>\n                <property name=\"shadow_type\">GTK_SHADOW_IN</property>\n                <child>\n                  <widget class=\"GtkAlignment\" id=\"alignment4\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"left_padding\">12</property>\n                    <child>\n                      <widget class=\"GtkVBox\" id=\"vbox9\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <child>\n                          <widget class=\"GtkFrame\" id=\"frame3\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label_xalign\">0</property>\n                            <property name=\"shadow_type\">GTK_SHADOW_NONE</property>\n                            <child>\n                              <widget class=\"GtkAlignment\" id=\"alignment5\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"left_padding\">12</property>\n                                <child>\n                                  <widget class=\"GtkTable\" id=\"table1\">\n                                    <property name=\"visible\">True</property>\n                                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                    <property name=\"n_rows\">3</property>\n                                    <property name=\"n_columns\">3</property>\n                                    <child>\n                                      <widget class=\"GtkLabel\" id=\"label10\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"label\" translatable=\"yes\">BPM:</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkLabel\" id=\"label11\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"label\" translatable=\"yes\">PQN:</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"top_attach\">1</property>\n                                        <property name=\"bottom_attach\">2</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkSpinButton\" id=\"spinbutton_bpm\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"adjustment\">0 0 240 1 10 10</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"left_attach\">1</property>\n                                        <property name=\"right_attach\">2</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkSpinButton\" id=\"spinbutton_pqn\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"adjustment\">0 0 10000 1 10 10</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"left_attach\">1</property>\n                                        <property name=\"right_attach\">2</property>\n                                        <property name=\"top_attach\">1</property>\n                                        <property name=\"bottom_attach\">2</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkButton\" id=\"button_update_bpm\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"receives_default\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"label\" translatable=\"yes\">Update</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"left_attach\">2</property>\n                                        <property name=\"right_attach\">3</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkButton\" id=\"button_update_pqn\">\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"receives_default\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"label\" translatable=\"yes\">Update</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"left_attach\">2</property>\n                                        <property name=\"right_attach\">3</property>\n                                        <property name=\"top_attach\">1</property>\n                                        <property name=\"bottom_attach\">2</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkLabel\" id=\"label30\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"label\" translatable=\"yes\">Song Name:</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"top_attach\">2</property>\n                                        <property name=\"bottom_attach\">3</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkEntry\" id=\"entry_sngnam\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"left_attach\">1</property>\n                                        <property name=\"right_attach\">2</property>\n                                        <property name=\"top_attach\">2</property>\n                                        <property name=\"bottom_attach\">3</property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkButton\" id=\"button_update_sngnam\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"receives_default\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"label\" translatable=\"yes\">Update</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"left_attach\">2</property>\n                                        <property name=\"right_attach\">3</property>\n                                        <property name=\"top_attach\">2</property>\n                                        <property name=\"bottom_attach\">3</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                  </widget>\n                                </child>\n                              </widget>\n                            </child>\n                            <child>\n                              <widget class=\"GtkLabel\" id=\"label8\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">&lt;i&gt;Song:&lt;/i&gt;</property>\n                                <property name=\"use_markup\">True</property>\n                              </widget>\n                              <packing>\n                                <property name=\"type\">label_item</property>\n                              </packing>\n                            </child>\n                          </widget>\n                          <packing>\n                            <property name=\"expand\">False</property>\n                            <property name=\"fill\">False</property>\n                          </packing>\n                        </child>\n                        <child>\n                          <widget class=\"GtkFrame\" id=\"frame4\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label_xalign\">0</property>\n                            <property name=\"shadow_type\">GTK_SHADOW_NONE</property>\n                            <child>\n                              <widget class=\"GtkAlignment\" id=\"alignment6\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"left_padding\">12</property>\n                                <child>\n                                  <widget class=\"GtkTable\" id=\"table2\">\n                                    <property name=\"visible\">True</property>\n                                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                    <property name=\"n_rows\">2</property>\n                                    <property name=\"n_columns\">2</property>\n                                    <child>\n                                      <widget class=\"GtkLabel\" id=\"label12\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"label\" translatable=\"yes\">Queue Delay:</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkLabel\" id=\"label13\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"label\" translatable=\"yes\">Timer Ticks:</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"top_attach\">1</property>\n                                        <property name=\"bottom_attach\">2</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkSpinButton\" id=\"spinbutton_qd\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"adjustment\">0 0 100 1 10 10</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"left_attach\">1</property>\n                                        <property name=\"right_attach\">2</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                    <child>\n                                      <widget class=\"GtkSpinButton\" id=\"spinbutton_tt\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"adjustment\">0 0 100 1 10 10</property>\n                                      </widget>\n                                      <packing>\n                                        <property name=\"left_attach\">1</property>\n                                        <property name=\"right_attach\">2</property>\n                                        <property name=\"top_attach\">1</property>\n                                        <property name=\"bottom_attach\">2</property>\n                                        <property name=\"x_options\"></property>\n                                        <property name=\"y_options\"></property>\n                                      </packing>\n                                    </child>\n                                  </widget>\n                                </child>\n                              </widget>\n                            </child>\n                            <child>\n                              <widget class=\"GtkLabel\" id=\"label9\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">&lt;i&gt;Sequencer:&lt;/i&gt; (will take effect on next \"Play\")</property>\n                                <property name=\"use_markup\">True</property>\n                              </widget>\n                              <packing>\n                                <property name=\"type\">label_item</property>\n                              </packing>\n                            </child>\n                          </widget>\n                          <packing>\n                            <property name=\"expand\">False</property>\n                            <property name=\"fill\">False</property>\n                            <property name=\"position\">1</property>\n                          </packing>\n                        </child>\n                      </widget>\n                    </child>\n                  </widget>\n                </child>\n                <child>\n                  <widget class=\"GtkLabel\" id=\"label7\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"label\" translatable=\"yes\">&lt;b&gt;Settings:&lt;/b&gt;</property>\n                    <property name=\"use_markup\">True</property>\n                  </widget>\n                  <packing>\n                    <property name=\"type\">label_item</property>\n                  </packing>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkFrame\" id=\"frame5\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label_xalign\">0</property>\n                <property name=\"shadow_type\">GTK_SHADOW_IN</property>\n                <child>\n                  <widget class=\"GtkAlignment\" id=\"alignment7\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"left_padding\">12</property>\n                    <child>\n                      <widget class=\"GtkVBox\" id=\"vbox10\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <child>\n                          <widget class=\"GtkTable\" id=\"table4\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"n_rows\">3</property>\n                            <property name=\"n_columns\">2</property>\n                            <child>\n                              <placeholder/>\n                            </child>\n                            <child>\n                              <placeholder/>\n                            </child>\n                            <child>\n                              <widget class=\"GtkLabel\" id=\"label15\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">&lt;i&gt;Playing Time:&lt;/i&gt;  </property>\n                                <property name=\"use_markup\">True</property>\n                              </widget>\n                              <packing>\n                                <property name=\"x_options\"></property>\n                                <property name=\"y_options\"></property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkLabel\" id=\"label_playing\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">&lt;tt&gt;--:--:--&lt;/tt&gt;</property>\n                                <property name=\"use_markup\">True</property>\n                                <property name=\"justify\">GTK_JUSTIFY_RIGHT</property>\n                              </widget>\n                              <packing>\n                                <property name=\"left_attach\">1</property>\n                                <property name=\"right_attach\">2</property>\n                                <property name=\"x_options\"></property>\n                                <property name=\"y_options\"></property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkLabel\" id=\"label19\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">&lt;i&gt;FileName:&lt;/i&gt;  </property>\n                                <property name=\"use_markup\">True</property>\n                              </widget>\n                              <packing>\n                                <property name=\"top_attach\">1</property>\n                                <property name=\"bottom_attach\">2</property>\n                                <property name=\"x_options\"></property>\n                                <property name=\"y_options\"></property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkLabel\" id=\"label_filename\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">&lt;tt&gt;/tmp/non_name.osk&lt;/tt&gt;</property>\n                                <property name=\"use_markup\">True</property>\n                                <property name=\"justify\">GTK_JUSTIFY_RIGHT</property>\n                                <property name=\"wrap\">True</property>\n                                <property name=\"wrap_mode\">PANGO_WRAP_WORD_CHAR</property>\n                                <property name=\"selectable\">True</property>\n                              </widget>\n                              <packing>\n                                <property name=\"left_attach\">1</property>\n                                <property name=\"right_attach\">2</property>\n                                <property name=\"top_attach\">1</property>\n                                <property name=\"bottom_attach\">2</property>\n                                <property name=\"x_options\"></property>\n                                <property name=\"y_options\"></property>\n                              </packing>\n                            </child>\n                          </widget>\n                          <packing>\n                            <property name=\"expand\">False</property>\n                            <property name=\"fill\">False</property>\n                          </packing>\n                        </child>\n                        <child>\n                          <placeholder/>\n                        </child>\n                        <child>\n                          <widget class=\"GtkFrame\" id=\"frame6\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label_xalign\">0</property>\n                            <property name=\"shadow_type\">GTK_SHADOW_NONE</property>\n                            <child>\n                              <widget class=\"GtkAlignment\" id=\"alignment8\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"left_padding\">12</property>\n                                <child>\n                                  <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow1\">\n                                    <property name=\"visible\">True</property>\n                                    <property name=\"can_focus\">True</property>\n                                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                    <property name=\"hscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                                    <property name=\"vscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                                    <child>\n                                      <widget class=\"GtkTextView\" id=\"textview_msg\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                        <property name=\"editable\">False</property>\n                                        <property name=\"wrap_mode\">GTK_WRAP_WORD</property>\n                                        <property name=\"cursor_visible\">False</property>\n                                        <property name=\"accepts_tab\">False</property>\n                                      </widget>\n                                    </child>\n                                  </widget>\n                                </child>\n                              </widget>\n                            </child>\n                            <child>\n                              <widget class=\"GtkLabel\" id=\"label16\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                                <property name=\"label\" translatable=\"yes\">&lt;i&gt;Messages:&lt;/i&gt;</property>\n                                <property name=\"use_markup\">True</property>\n                              </widget>\n                              <packing>\n                                <property name=\"type\">label_item</property>\n                              </packing>\n                            </child>\n                          </widget>\n                          <packing>\n                            <property name=\"position\">2</property>\n                          </packing>\n                        </child>\n                      </widget>\n                    </child>\n                  </widget>\n                </child>\n                <child>\n                  <widget class=\"GtkLabel\" id=\"label14\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"label\" translatable=\"yes\">&lt;b&gt;Informations:&lt;/b&gt;</property>\n                    <property name=\"use_markup\">True</property>\n                  </widget>\n                  <packing>\n                    <property name=\"type\">label_item</property>\n                  </packing>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"position\">3</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkFrame\" id=\"frame7\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <property name=\"label_xalign\">0</property>\n            <property name=\"shadow_type\">GTK_SHADOW_IN</property>\n            <child>\n              <widget class=\"GtkAlignment\" id=\"alignment9\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"left_padding\">12</property>\n                <child>\n                  <widget class=\"GtkVBox\" id=\"vbox11\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <child>\n                      <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow2\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <property name=\"hscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                        <property name=\"vscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                        <child>\n                          <widget class=\"GtkTreeView\" id=\"treeview_midi\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"headers_visible\">False</property>\n                            <property name=\"enable_search\">False</property>\n                            <property name=\"enable_grid_lines\">GTK_TREE_VIEW_GRID_LINES_BOTH</property>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                    <child>\n                      <widget class=\"GtkHBox\" id=\"hbox9\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_add_midi\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Add</property>\n                          </widget>\n                        </child>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_edit_midi\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Edit</property>\n                          </widget>\n                          <packing>\n                            <property name=\"position\">1</property>\n                          </packing>\n                        </child>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_suppr_midi\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Suppr</property>\n                          </widget>\n                          <packing>\n                            <property name=\"position\">2</property>\n                          </packing>\n                        </child>\n                        <child>\n                          <placeholder/>\n                        </child>\n                        <child>\n                          <placeholder/>\n                        </child>\n                      </widget>\n                      <packing>\n                        <property name=\"expand\">False</property>\n                        <property name=\"position\">1</property>\n                      </packing>\n                    </child>\n                  </widget>\n                </child>\n              </widget>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label21\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">&lt;b&gt;Midi Tracks:&lt;/b&gt;</property>\n                <property name=\"use_markup\">True</property>\n              </widget>\n              <packing>\n                <property name=\"type\">label_item</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkFrame\" id=\"frame10\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <property name=\"label_xalign\">0</property>\n            <property name=\"shadow_type\">GTK_SHADOW_IN</property>\n            <child>\n              <widget class=\"GtkAlignment\" id=\"alignment12\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"left_padding\">12</property>\n                <child>\n                  <widget class=\"GtkVBox\" id=\"vbox13\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <child>\n                      <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow4\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <property name=\"hscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                        <property name=\"vscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                        <child>\n                          <widget class=\"GtkTreeView\" id=\"treeview_meta\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"headers_visible\">False</property>\n                            <property name=\"enable_search\">False</property>\n                            <property name=\"enable_grid_lines\">GTK_TREE_VIEW_GRID_LINES_BOTH</property>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                    <child>\n                      <widget class=\"GtkHBox\" id=\"hbox11\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_add_meta\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Add</property>\n                          </widget>\n                        </child>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_edit_meta\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Edit</property>\n                          </widget>\n                          <packing>\n                            <property name=\"position\">1</property>\n                          </packing>\n                        </child>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_suppr_meta\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Suppr</property>\n                          </widget>\n                          <packing>\n                            <property name=\"position\">2</property>\n                          </packing>\n                        </child>\n                        <child>\n                          <placeholder/>\n                        </child>\n                        <child>\n                          <placeholder/>\n                        </child>\n                      </widget>\n                      <packing>\n                        <property name=\"expand\">False</property>\n                        <property name=\"position\">1</property>\n                      </packing>\n                    </child>\n                  </widget>\n                </child>\n              </widget>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label24\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">&lt;b&gt;Meta Tracks:&lt;/b&gt;</property>\n                <property name=\"use_markup\">True</property>\n              </widget>\n              <packing>\n                <property name=\"type\">label_item</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"position\">2</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkFrame\" id=\"frame11\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <property name=\"label_xalign\">0</property>\n            <property name=\"shadow_type\">GTK_SHADOW_IN</property>\n            <child>\n              <widget class=\"GtkAlignment\" id=\"alignment13\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"left_padding\">12</property>\n                <child>\n                  <widget class=\"GtkVBox\" id=\"vbox14\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <child>\n                      <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow5\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <property name=\"hscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                        <property name=\"vscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                        <child>\n                          <widget class=\"GtkTreeView\" id=\"treeview_iact\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"headers_visible\">False</property>\n                            <property name=\"enable_search\">False</property>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                    <child>\n                      <widget class=\"GtkHBox\" id=\"hbox12\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_add_iact\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Add</property>\n                          </widget>\n                        </child>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_edit_iact\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Edit</property>\n                          </widget>\n                          <packing>\n                            <property name=\"position\">1</property>\n                          </packing>\n                        </child>\n                        <child>\n                          <widget class=\"GtkButton\" id=\"button_suppr_iact\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"receives_default\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"label\" translatable=\"yes\">Suppr</property>\n                          </widget>\n                          <packing>\n                            <property name=\"position\">2</property>\n                          </packing>\n                        </child>\n                        <child>\n                          <placeholder/>\n                        </child>\n                        <child>\n                          <placeholder/>\n                        </child>\n                      </widget>\n                      <packing>\n                        <property name=\"expand\">False</property>\n                        <property name=\"position\">1</property>\n                      </packing>\n                    </child>\n                  </widget>\n                </child>\n              </widget>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label25\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">&lt;b&gt;Interaction:&lt;/b&gt;</property>\n                <property name=\"use_markup\">True</property>\n              </widget>\n              <packing>\n                <property name=\"type\">label_item</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"position\">3</property>\n          </packing>\n        </child>\n      </widget>\n    </child>\n  </widget>\n  <widget class=\"GtkWindow\" id=\"iact_window\">\n    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n    <property name=\"title\" translatable=\"yes\">Edit/Create Interactions</property>\n    <property name=\"window_position\">GTK_WIN_POS_CENTER_ALWAYS</property>\n    <child>\n      <widget class=\"GtkVBox\" id=\"vbox12\">\n        <property name=\"visible\">True</property>\n        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox_msg\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label_msg\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"ypad\">10</property>\n                <property name=\"label\" translatable=\"yes\">&lt;b&gt;Edit/Create your input handler:&lt;/b&gt;</property>\n                <property name=\"use_markup\">True</property>\n              </widget>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">False</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkTable\" id=\"table5\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <property name=\"n_rows\">2</property>\n            <property name=\"n_columns\">2</property>\n            <child>\n              <widget class=\"GtkHBox\" id=\"hbox_action\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"left_attach\">1</property>\n                <property name=\"right_attach\">2</property>\n                <property name=\"top_attach\">1</property>\n                <property name=\"bottom_attach\">2</property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkHBox\" id=\"hbox_input\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"left_attach\">1</property>\n                <property name=\"right_attach\">2</property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label18\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Action:</property>\n              </widget>\n              <packing>\n                <property name=\"top_attach\">1</property>\n                <property name=\"bottom_attach\">2</property>\n                <property name=\"x_options\"></property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label17\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Input:</property>\n              </widget>\n              <packing>\n                <property name=\"x_options\"></property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">False</property>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox19\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_ok\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">OK</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"pack_type\">GTK_PACK_END</property>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_cancel\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Cancel</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"pack_type\">GTK_PACK_END</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"pack_type\">GTK_PACK_END</property>\n            <property name=\"position\">2</property>\n          </packing>\n        </child>\n      </widget>\n    </child>\n  </widget>\n  <widget class=\"GtkWindow\" id=\"midi_window\">\n    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n    <property name=\"title\" translatable=\"yes\">Edit Midi Track</property>\n    <property name=\"window_position\">GTK_WIN_POS_CENTER_ALWAYS</property>\n    <property name=\"destroy_with_parent\">True</property>\n    <child>\n      <widget class=\"GtkVBox\" id=\"vbox15\">\n        <property name=\"visible\">True</property>\n        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox13\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label_msg\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">&lt;b&gt;Edit this midi track:&lt;/b&gt;</property>\n                <property name=\"use_markup\">True</property>\n              </widget>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">False</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkTable\" id=\"table6\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <property name=\"n_rows\">4</property>\n            <property name=\"n_columns\">2</property>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <widget class=\"GtkHBox\" id=\"hbox_length\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <child>\n                  <widget class=\"GtkSpinButton\" id=\"spinbutton_lgth\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"can_focus\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"adjustment\">0 0 999 1 10 10</property>\n                  </widget>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"left_attach\">1</property>\n                <property name=\"right_attach\">2</property>\n                <property name=\"top_attach\">2</property>\n                <property name=\"bottom_attach\">3</property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkHBox\" id=\"hbox_port\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"left_attach\">1</property>\n                <property name=\"right_attach\">2</property>\n                <property name=\"top_attach\">1</property>\n                <property name=\"bottom_attach\">2</property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkEntry\" id=\"entry_name\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n              </widget>\n              <packing>\n                <property name=\"left_attach\">1</property>\n                <property name=\"right_attach\">2</property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label27\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Length:</property>\n              </widget>\n              <packing>\n                <property name=\"top_attach\">2</property>\n                <property name=\"bottom_attach\">3</property>\n                <property name=\"x_options\"></property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label26\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Port:</property>\n              </widget>\n              <packing>\n                <property name=\"top_attach\">1</property>\n                <property name=\"bottom_attach\">2</property>\n                <property name=\"x_options\"></property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label20\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Name:</property>\n              </widget>\n              <packing>\n                <property name=\"x_options\"></property>\n                <property name=\"y_options\"></property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox18\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_ok\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">OK</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"pack_type\">GTK_PACK_END</property>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_cancel\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Cancel</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"pack_type\">GTK_PACK_END</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"position\">2</property>\n          </packing>\n        </child>\n      </widget>\n    </child>\n  </widget>\n  <widget class=\"GtkWindow\" id=\"meta_window\">\n    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n    <property name=\"title\" translatable=\"yes\">Edit/Create a Meta Track</property>\n    <property name=\"window_position\">GTK_WIN_POS_CENTER_ALWAYS</property>\n    <property name=\"default_width\">589</property>\n    <property name=\"default_height\">430</property>\n    <child>\n      <widget class=\"GtkVBox\" id=\"vbox16\">\n        <property name=\"visible\">True</property>\n        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox15\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label_msg3\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">&lt;b&gt;Edit this meta track:   &lt;/b&gt;</property>\n                <property name=\"use_markup\">True</property>\n              </widget>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">False</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkVBox\" id=\"vbox17\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <widget class=\"GtkFrame\" id=\"frame8\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label_xalign\">0</property>\n                <property name=\"shadow_type\">GTK_SHADOW_NONE</property>\n                <child>\n                  <widget class=\"GtkAlignment\" id=\"alignment10\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"left_padding\">12</property>\n                    <child>\n                      <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow3\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                        <property name=\"hscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                        <property name=\"vscrollbar_policy\">GTK_POLICY_AUTOMATIC</property>\n                        <child>\n                          <widget class=\"GtkTreeView\" id=\"treeview\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                            <property name=\"headers_visible\">False</property>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                  </widget>\n                </child>\n                <child>\n                  <widget class=\"GtkLabel\" id=\"label28\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"label\" translatable=\"yes\">&lt;b&gt;Meta Events&lt;/b&gt;</property>\n                    <property name=\"use_markup\">True</property>\n                  </widget>\n                  <packing>\n                    <property name=\"type\">label_item</property>\n                  </packing>\n                </child>\n              </widget>\n            </child>\n            <child>\n              <widget class=\"GtkHBox\" id=\"hbox16\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <child>\n                  <widget class=\"GtkButton\" id=\"button_add\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"can_focus\">True</property>\n                    <property name=\"receives_default\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"label\" translatable=\"yes\">Add</property>\n                  </widget>\n                  <packing>\n                    <property name=\"expand\">False</property>\n                    <property name=\"fill\">False</property>\n                  </packing>\n                </child>\n                <child>\n                  <widget class=\"GtkButton\" id=\"button_suppr\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"can_focus\">True</property>\n                    <property name=\"receives_default\">True</property>\n                    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                    <property name=\"label\" translatable=\"yes\">Suppr</property>\n                  </widget>\n                  <packing>\n                    <property name=\"expand\">False</property>\n                    <property name=\"fill\">False</property>\n                    <property name=\"position\">1</property>\n                  </packing>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkHBox\" id=\"hbox_edit\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n                <child>\n                  <placeholder/>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox_name_lgth\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label22\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Name:</property>\n              </widget>\n            </child>\n            <child>\n              <widget class=\"GtkEntry\" id=\"entry_name\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n              </widget>\n              <packing>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkLabel\" id=\"label23\">\n                <property name=\"visible\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Length:</property>\n              </widget>\n              <packing>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkSpinButton\" id=\"spinbutton_lgth\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"adjustment\">0 0 100000 1 10 10</property>\n              </widget>\n              <packing>\n                <property name=\"position\">3</property>\n              </packing>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">False</property>\n            <property name=\"position\">2</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox10\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_ok\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">OK</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"pack_type\">GTK_PACK_END</property>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_cancel\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Cancel</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"pack_type\">GTK_PACK_END</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">False</property>\n            <property name=\"position\">3</property>\n          </packing>\n        </child>\n      </widget>\n    </child>\n  </widget>\n  <widget class=\"GtkDialog\" id=\"dialog_notsaved\">\n    <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n    <property name=\"border_width\">5</property>\n    <property name=\"modal\">True</property>\n    <property name=\"window_position\">GTK_WIN_POS_CENTER_ALWAYS</property>\n    <property name=\"type_hint\">GDK_WINDOW_TYPE_HINT_DIALOG</property>\n    <property name=\"has_separator\">False</property>\n    <child internal-child=\"vbox\">\n      <widget class=\"GtkVBox\" id=\"dialog-vbox3\">\n        <property name=\"visible\">True</property>\n        <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n        <property name=\"spacing\">2</property>\n        <child>\n          <widget class=\"GtkLabel\" id=\"label_msg\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <property name=\"label\" translatable=\"yes\">There seems to be some not saved changes, do you want to save before proceeding ?</property>\n            <property name=\"wrap\">True</property>\n          </widget>\n          <packing>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n        <child internal-child=\"action_area\">\n          <widget class=\"GtkHButtonBox\" id=\"dialog-action_area3\">\n            <property name=\"visible\">True</property>\n            <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n            <property name=\"layout_style\">GTK_BUTTONBOX_END</property>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_ok\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Save &amp; Continue</property>\n              </widget>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_dsq\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Don't Save &amp; Continue</property>\n              </widget>\n              <packing>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"button_cancel\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"events\">GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK</property>\n                <property name=\"label\" translatable=\"yes\">Don't Continue</property>\n              </widget>\n              <packing>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"pack_type\">GTK_PACK_END</property>\n          </packing>\n        </child>\n      </widget>\n    </child>\n  </widget>\n</glade-interface>\n"

class app_window ?domain ?autoconnect(*=true*) () =
  let xmldata = Glade.create ~data  ~root:"app_window" ?domain () in
  object (self)
    inherit Glade.xml ?autoconnect xmldata
    val toplevel =
      new GWindow.window (GtkWindow.Window.cast
        (Glade.get_widget_msg ~name:"app_window" ~info:"GtkWindow" xmldata))
    method toplevel = toplevel
    val app_window =
      new GWindow.window (GtkWindow.Window.cast
        (Glade.get_widget_msg ~name:"app_window" ~info:"GtkWindow" xmldata))
    method app_window = app_window
    val hbox6 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox6" ~info:"GtkHBox" xmldata))
    method hbox6 = hbox6
    val vbox7 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox7" ~info:"GtkVBox" xmldata))
    method vbox7 = vbox7
    val label_title =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label_title" ~info:"GtkLabel" xmldata))
    method label_title = label_title
    val frame1 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame1" ~info:"GtkFrame" xmldata))
    method frame1 = frame1
    val alignment3 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment3" ~info:"GtkAlignment" xmldata))
    method alignment3 = alignment3
    val hbox7 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox7" ~info:"GtkHBox" xmldata))
    method hbox7 = hbox7
    val table3 =
      new GPack.table (GtkPack.Table.cast
        (Glade.get_widget_msg ~name:"table3" ~info:"GtkTable" xmldata))
    method table3 = table3
    val button_new =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_new" ~info:"GtkButton" xmldata))
    method button_new = button_new
    val button_open =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_open" ~info:"GtkButton" xmldata))
    method button_open = button_open
    val button_save =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_save" ~info:"GtkButton" xmldata))
    method button_save = button_save
    val button_saveas =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_saveas" ~info:"GtkButton" xmldata))
    method button_saveas = button_saveas
    val button_quit =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_quit" ~info:"GtkButton" xmldata))
    method button_quit = button_quit
    val button_help =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_help" ~info:"GtkButton" xmldata))
    method button_help = button_help
    val vbox8 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox8" ~info:"GtkVBox" xmldata))
    method vbox8 = vbox8
    val button_play =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_play" ~info:"GtkButton" xmldata))
    method button_play = button_play
    val button_stop =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_stop" ~info:"GtkButton" xmldata))
    method button_stop = button_stop
    val label6 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label6" ~info:"GtkLabel" xmldata))
    method label6 = label6
    val frame2 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame2" ~info:"GtkFrame" xmldata))
    method frame2 = frame2
    val alignment4 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment4" ~info:"GtkAlignment" xmldata))
    method alignment4 = alignment4
    val vbox9 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox9" ~info:"GtkVBox" xmldata))
    method vbox9 = vbox9
    val frame3 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame3" ~info:"GtkFrame" xmldata))
    method frame3 = frame3
    val alignment5 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment5" ~info:"GtkAlignment" xmldata))
    method alignment5 = alignment5
    val table1 =
      new GPack.table (GtkPack.Table.cast
        (Glade.get_widget_msg ~name:"table1" ~info:"GtkTable" xmldata))
    method table1 = table1
    val label10 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label10" ~info:"GtkLabel" xmldata))
    method label10 = label10
    val label11 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label11" ~info:"GtkLabel" xmldata))
    method label11 = label11
    val spinbutton_bpm =
      new GEdit.spin_button (GtkEdit.SpinButton.cast
        (Glade.get_widget_msg ~name:"spinbutton_bpm" ~info:"GtkSpinButton" xmldata))
    method spinbutton_bpm = spinbutton_bpm
    val spinbutton_pqn =
      new GEdit.spin_button (GtkEdit.SpinButton.cast
        (Glade.get_widget_msg ~name:"spinbutton_pqn" ~info:"GtkSpinButton" xmldata))
    method spinbutton_pqn = spinbutton_pqn
    val button_update_bpm =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_update_bpm" ~info:"GtkButton" xmldata))
    method button_update_bpm = button_update_bpm
    val button_update_pqn =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_update_pqn" ~info:"GtkButton" xmldata))
    method button_update_pqn = button_update_pqn
    val label30 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label30" ~info:"GtkLabel" xmldata))
    method label30 = label30
    val entry_sngnam =
      new GEdit.entry (GtkEdit.Entry.cast
        (Glade.get_widget_msg ~name:"entry_sngnam" ~info:"GtkEntry" xmldata))
    method entry_sngnam = entry_sngnam
    val button_update_sngnam =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_update_sngnam" ~info:"GtkButton" xmldata))
    method button_update_sngnam = button_update_sngnam
    val label8 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label8" ~info:"GtkLabel" xmldata))
    method label8 = label8
    val frame4 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame4" ~info:"GtkFrame" xmldata))
    method frame4 = frame4
    val alignment6 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment6" ~info:"GtkAlignment" xmldata))
    method alignment6 = alignment6
    val table2 =
      new GPack.table (GtkPack.Table.cast
        (Glade.get_widget_msg ~name:"table2" ~info:"GtkTable" xmldata))
    method table2 = table2
    val label12 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label12" ~info:"GtkLabel" xmldata))
    method label12 = label12
    val label13 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label13" ~info:"GtkLabel" xmldata))
    method label13 = label13
    val spinbutton_qd =
      new GEdit.spin_button (GtkEdit.SpinButton.cast
        (Glade.get_widget_msg ~name:"spinbutton_qd" ~info:"GtkSpinButton" xmldata))
    method spinbutton_qd = spinbutton_qd
    val spinbutton_tt =
      new GEdit.spin_button (GtkEdit.SpinButton.cast
        (Glade.get_widget_msg ~name:"spinbutton_tt" ~info:"GtkSpinButton" xmldata))
    method spinbutton_tt = spinbutton_tt
    val label9 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label9" ~info:"GtkLabel" xmldata))
    method label9 = label9
    val label7 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label7" ~info:"GtkLabel" xmldata))
    method label7 = label7
    val frame5 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame5" ~info:"GtkFrame" xmldata))
    method frame5 = frame5
    val alignment7 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment7" ~info:"GtkAlignment" xmldata))
    method alignment7 = alignment7
    val vbox10 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox10" ~info:"GtkVBox" xmldata))
    method vbox10 = vbox10
    val table4 =
      new GPack.table (GtkPack.Table.cast
        (Glade.get_widget_msg ~name:"table4" ~info:"GtkTable" xmldata))
    method table4 = table4
    val label15 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label15" ~info:"GtkLabel" xmldata))
    method label15 = label15
    val label_playing =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label_playing" ~info:"GtkLabel" xmldata))
    method label_playing = label_playing
    val label19 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label19" ~info:"GtkLabel" xmldata))
    method label19 = label19
    val label_filename =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label_filename" ~info:"GtkLabel" xmldata))
    method label_filename = label_filename
    val frame6 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame6" ~info:"GtkFrame" xmldata))
    method frame6 = frame6
    val alignment8 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment8" ~info:"GtkAlignment" xmldata))
    method alignment8 = alignment8
    val scrolledwindow1 =
      new GBin.scrolled_window (GtkBin.ScrolledWindow.cast
        (Glade.get_widget_msg ~name:"scrolledwindow1" ~info:"GtkScrolledWindow" xmldata))
    method scrolledwindow1 = scrolledwindow1
    val textview_msg =
      new GText.view (GtkText.View.cast
        (Glade.get_widget_msg ~name:"textview_msg" ~info:"GtkTextView" xmldata))
    method textview_msg = textview_msg
    val label16 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label16" ~info:"GtkLabel" xmldata))
    method label16 = label16
    val label14 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label14" ~info:"GtkLabel" xmldata))
    method label14 = label14
    val frame7 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame7" ~info:"GtkFrame" xmldata))
    method frame7 = frame7
    val alignment9 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment9" ~info:"GtkAlignment" xmldata))
    method alignment9 = alignment9
    val vbox11 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox11" ~info:"GtkVBox" xmldata))
    method vbox11 = vbox11
    val scrolledwindow2 =
      new GBin.scrolled_window (GtkBin.ScrolledWindow.cast
        (Glade.get_widget_msg ~name:"scrolledwindow2" ~info:"GtkScrolledWindow" xmldata))
    method scrolledwindow2 = scrolledwindow2
    val treeview_midi =
      new GTree.view (GtkTree.TreeView.cast
        (Glade.get_widget_msg ~name:"treeview_midi" ~info:"GtkTreeView" xmldata))
    method treeview_midi = treeview_midi
    val hbox9 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox9" ~info:"GtkHBox" xmldata))
    method hbox9 = hbox9
    val button_add_midi =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_add_midi" ~info:"GtkButton" xmldata))
    method button_add_midi = button_add_midi
    val button_edit_midi =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_edit_midi" ~info:"GtkButton" xmldata))
    method button_edit_midi = button_edit_midi
    val button_suppr_midi =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_suppr_midi" ~info:"GtkButton" xmldata))
    method button_suppr_midi = button_suppr_midi
    val label21 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label21" ~info:"GtkLabel" xmldata))
    method label21 = label21
    val frame10 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame10" ~info:"GtkFrame" xmldata))
    method frame10 = frame10
    val alignment12 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment12" ~info:"GtkAlignment" xmldata))
    method alignment12 = alignment12
    val vbox13 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox13" ~info:"GtkVBox" xmldata))
    method vbox13 = vbox13
    val scrolledwindow4 =
      new GBin.scrolled_window (GtkBin.ScrolledWindow.cast
        (Glade.get_widget_msg ~name:"scrolledwindow4" ~info:"GtkScrolledWindow" xmldata))
    method scrolledwindow4 = scrolledwindow4
    val treeview_meta =
      new GTree.view (GtkTree.TreeView.cast
        (Glade.get_widget_msg ~name:"treeview_meta" ~info:"GtkTreeView" xmldata))
    method treeview_meta = treeview_meta
    val hbox11 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox11" ~info:"GtkHBox" xmldata))
    method hbox11 = hbox11
    val button_add_meta =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_add_meta" ~info:"GtkButton" xmldata))
    method button_add_meta = button_add_meta
    val button_edit_meta =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_edit_meta" ~info:"GtkButton" xmldata))
    method button_edit_meta = button_edit_meta
    val button_suppr_meta =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_suppr_meta" ~info:"GtkButton" xmldata))
    method button_suppr_meta = button_suppr_meta
    val label24 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label24" ~info:"GtkLabel" xmldata))
    method label24 = label24
    val frame11 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame11" ~info:"GtkFrame" xmldata))
    method frame11 = frame11
    val alignment13 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment13" ~info:"GtkAlignment" xmldata))
    method alignment13 = alignment13
    val vbox14 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox14" ~info:"GtkVBox" xmldata))
    method vbox14 = vbox14
    val scrolledwindow5 =
      new GBin.scrolled_window (GtkBin.ScrolledWindow.cast
        (Glade.get_widget_msg ~name:"scrolledwindow5" ~info:"GtkScrolledWindow" xmldata))
    method scrolledwindow5 = scrolledwindow5
    val treeview_iact =
      new GTree.view (GtkTree.TreeView.cast
        (Glade.get_widget_msg ~name:"treeview_iact" ~info:"GtkTreeView" xmldata))
    method treeview_iact = treeview_iact
    val hbox12 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox12" ~info:"GtkHBox" xmldata))
    method hbox12 = hbox12
    val button_add_iact =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_add_iact" ~info:"GtkButton" xmldata))
    method button_add_iact = button_add_iact
    val button_edit_iact =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_edit_iact" ~info:"GtkButton" xmldata))
    method button_edit_iact = button_edit_iact
    val button_suppr_iact =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_suppr_iact" ~info:"GtkButton" xmldata))
    method button_suppr_iact = button_suppr_iact
    val label25 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label25" ~info:"GtkLabel" xmldata))
    method label25 = label25
    method reparent parent =
      hbox6#misc#reparent parent;
      toplevel#destroy ()
    method check_widgets () = ()
  end
class iact_window ?domain ?autoconnect(*=true*) () =
  let xmldata = Glade.create ~data  ~root:"iact_window" ?domain () in
  object (self)
    inherit Glade.xml ?autoconnect xmldata
    val toplevel =
      new GWindow.window (GtkWindow.Window.cast
        (Glade.get_widget_msg ~name:"iact_window" ~info:"GtkWindow" xmldata))
    method toplevel = toplevel
    val iact_window =
      new GWindow.window (GtkWindow.Window.cast
        (Glade.get_widget_msg ~name:"iact_window" ~info:"GtkWindow" xmldata))
    method iact_window = iact_window
    val vbox12 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox12" ~info:"GtkVBox" xmldata))
    method vbox12 = vbox12
    val hbox_msg =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox_msg" ~info:"GtkHBox" xmldata))
    method hbox_msg = hbox_msg
    val label_msg =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label_msg" ~info:"GtkLabel" xmldata))
    method label_msg = label_msg
    val table5 =
      new GPack.table (GtkPack.Table.cast
        (Glade.get_widget_msg ~name:"table5" ~info:"GtkTable" xmldata))
    method table5 = table5
    val hbox_action =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox_action" ~info:"GtkHBox" xmldata))
    method hbox_action = hbox_action
    val hbox_input =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox_input" ~info:"GtkHBox" xmldata))
    method hbox_input = hbox_input
    val label18 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label18" ~info:"GtkLabel" xmldata))
    method label18 = label18
    val label17 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label17" ~info:"GtkLabel" xmldata))
    method label17 = label17
    val hbox19 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox19" ~info:"GtkHBox" xmldata))
    method hbox19 = hbox19
    val button_ok =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_ok" ~info:"GtkButton" xmldata))
    method button_ok = button_ok
    val button_cancel =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_cancel" ~info:"GtkButton" xmldata))
    method button_cancel = button_cancel
    method reparent parent =
      vbox12#misc#reparent parent;
      toplevel#destroy ()
    method check_widgets () = ()
  end
class midi_window ?domain ?autoconnect(*=true*) () =
  let xmldata = Glade.create ~data  ~root:"midi_window" ?domain () in
  object (self)
    inherit Glade.xml ?autoconnect xmldata
    val toplevel =
      new GWindow.window (GtkWindow.Window.cast
        (Glade.get_widget_msg ~name:"midi_window" ~info:"GtkWindow" xmldata))
    method toplevel = toplevel
    val midi_window =
      new GWindow.window (GtkWindow.Window.cast
        (Glade.get_widget_msg ~name:"midi_window" ~info:"GtkWindow" xmldata))
    method midi_window = midi_window
    val vbox15 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox15" ~info:"GtkVBox" xmldata))
    method vbox15 = vbox15
    val hbox13 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox13" ~info:"GtkHBox" xmldata))
    method hbox13 = hbox13
    val label_msg =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label_msg" ~info:"GtkLabel" xmldata))
    method label_msg = label_msg
    val table6 =
      new GPack.table (GtkPack.Table.cast
        (Glade.get_widget_msg ~name:"table6" ~info:"GtkTable" xmldata))
    method table6 = table6
    val hbox_length =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox_length" ~info:"GtkHBox" xmldata))
    method hbox_length = hbox_length
    val spinbutton_lgth =
      new GEdit.spin_button (GtkEdit.SpinButton.cast
        (Glade.get_widget_msg ~name:"spinbutton_lgth" ~info:"GtkSpinButton" xmldata))
    method spinbutton_lgth = spinbutton_lgth
    val hbox_port =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox_port" ~info:"GtkHBox" xmldata))
    method hbox_port = hbox_port
    val entry_name =
      new GEdit.entry (GtkEdit.Entry.cast
        (Glade.get_widget_msg ~name:"entry_name" ~info:"GtkEntry" xmldata))
    method entry_name = entry_name
    val label27 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label27" ~info:"GtkLabel" xmldata))
    method label27 = label27
    val label26 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label26" ~info:"GtkLabel" xmldata))
    method label26 = label26
    val label20 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label20" ~info:"GtkLabel" xmldata))
    method label20 = label20
    val hbox18 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox18" ~info:"GtkHBox" xmldata))
    method hbox18 = hbox18
    val button_ok =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_ok" ~info:"GtkButton" xmldata))
    method button_ok = button_ok
    val button_cancel =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_cancel" ~info:"GtkButton" xmldata))
    method button_cancel = button_cancel
    method reparent parent =
      vbox15#misc#reparent parent;
      toplevel#destroy ()
    method check_widgets () = ()
  end
class meta_window ?domain ?autoconnect(*=true*) () =
  let xmldata = Glade.create ~data  ~root:"meta_window" ?domain () in
  object (self)
    inherit Glade.xml ?autoconnect xmldata
    val toplevel =
      new GWindow.window (GtkWindow.Window.cast
        (Glade.get_widget_msg ~name:"meta_window" ~info:"GtkWindow" xmldata))
    method toplevel = toplevel
    val meta_window =
      new GWindow.window (GtkWindow.Window.cast
        (Glade.get_widget_msg ~name:"meta_window" ~info:"GtkWindow" xmldata))
    method meta_window = meta_window
    val vbox16 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox16" ~info:"GtkVBox" xmldata))
    method vbox16 = vbox16
    val hbox15 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox15" ~info:"GtkHBox" xmldata))
    method hbox15 = hbox15
    val label_msg3 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label_msg3" ~info:"GtkLabel" xmldata))
    method label_msg3 = label_msg3
    val vbox17 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"vbox17" ~info:"GtkVBox" xmldata))
    method vbox17 = vbox17
    val frame8 =
      new GBin.frame (GtkBin.Frame.cast
        (Glade.get_widget_msg ~name:"frame8" ~info:"GtkFrame" xmldata))
    method frame8 = frame8
    val alignment10 =
      new GBin.alignment (GtkBin.Alignment.cast
        (Glade.get_widget_msg ~name:"alignment10" ~info:"GtkAlignment" xmldata))
    method alignment10 = alignment10
    val scrolledwindow3 =
      new GBin.scrolled_window (GtkBin.ScrolledWindow.cast
        (Glade.get_widget_msg ~name:"scrolledwindow3" ~info:"GtkScrolledWindow" xmldata))
    method scrolledwindow3 = scrolledwindow3
    val treeview =
      new GTree.view (GtkTree.TreeView.cast
        (Glade.get_widget_msg ~name:"treeview" ~info:"GtkTreeView" xmldata))
    method treeview = treeview
    val label28 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label28" ~info:"GtkLabel" xmldata))
    method label28 = label28
    val hbox16 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox16" ~info:"GtkHBox" xmldata))
    method hbox16 = hbox16
    val button_add =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_add" ~info:"GtkButton" xmldata))
    method button_add = button_add
    val button_suppr =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_suppr" ~info:"GtkButton" xmldata))
    method button_suppr = button_suppr
    val hbox_edit =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox_edit" ~info:"GtkHBox" xmldata))
    method hbox_edit = hbox_edit
    val hbox_name_lgth =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox_name_lgth" ~info:"GtkHBox" xmldata))
    method hbox_name_lgth = hbox_name_lgth
    val label22 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label22" ~info:"GtkLabel" xmldata))
    method label22 = label22
    val entry_name =
      new GEdit.entry (GtkEdit.Entry.cast
        (Glade.get_widget_msg ~name:"entry_name" ~info:"GtkEntry" xmldata))
    method entry_name = entry_name
    val label23 =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label23" ~info:"GtkLabel" xmldata))
    method label23 = label23
    val spinbutton_lgth =
      new GEdit.spin_button (GtkEdit.SpinButton.cast
        (Glade.get_widget_msg ~name:"spinbutton_lgth" ~info:"GtkSpinButton" xmldata))
    method spinbutton_lgth = spinbutton_lgth
    val hbox10 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"hbox10" ~info:"GtkHBox" xmldata))
    method hbox10 = hbox10
    val button_ok =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_ok" ~info:"GtkButton" xmldata))
    method button_ok = button_ok
    val button_cancel =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_cancel" ~info:"GtkButton" xmldata))
    method button_cancel = button_cancel
    method reparent parent =
      vbox16#misc#reparent parent;
      toplevel#destroy ()
    method check_widgets () = ()
  end
class dialog_notsaved ?domain ?autoconnect(*=true*) () =
  let xmldata = Glade.create ~data  ~root:"dialog_notsaved" ?domain () in
  object (self)
    inherit Glade.xml ?autoconnect xmldata
    val toplevel =
      new GWindow.dialog_any (GtkWindow.Dialog.cast
        (Glade.get_widget_msg ~name:"dialog_notsaved" ~info:"GtkDialog" xmldata))
    method toplevel = toplevel
    val dialog_notsaved =
      new GWindow.dialog_any (GtkWindow.Dialog.cast
        (Glade.get_widget_msg ~name:"dialog_notsaved" ~info:"GtkDialog" xmldata))
    method dialog_notsaved = dialog_notsaved
    val dialog_vbox3 =
      new GPack.box (GtkPack.Box.cast
        (Glade.get_widget_msg ~name:"dialog-vbox3" ~info:"GtkVBox" xmldata))
    method dialog_vbox3 = dialog_vbox3
    val label_msg =
      new GMisc.label (GtkMisc.Label.cast
        (Glade.get_widget_msg ~name:"label_msg" ~info:"GtkLabel" xmldata))
    method label_msg = label_msg
    val dialog_action_area3 =
      new GPack.button_box (GtkPack.BBox.cast
        (Glade.get_widget_msg ~name:"dialog-action_area3" ~info:"GtkHButtonBox" xmldata))
    method dialog_action_area3 = dialog_action_area3
    val button_ok =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_ok" ~info:"GtkButton" xmldata))
    method button_ok = button_ok
    val button_dsq =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_dsq" ~info:"GtkButton" xmldata))
    method button_dsq = button_dsq
    val button_cancel =
      new GButton.button (GtkButton.Button.cast
        (Glade.get_widget_msg ~name:"button_cancel" ~info:"GtkButton" xmldata))
    method button_cancel = button_cancel
    method reparent parent =
      dialog_vbox3#misc#reparent parent;
      toplevel#destroy ()
    method check_widgets () = ()
  end

let check_all ?(show=false) () =
  ignore (GMain.Main.init ());
  let dialog_notsaved = new dialog_notsaved () in
  if show then dialog_notsaved#toplevel#show ();
  dialog_notsaved#check_widgets ();
  let meta_window = new meta_window () in
  if show then meta_window#toplevel#show ();
  meta_window#check_widgets ();
  let midi_window = new midi_window () in
  if show then midi_window#toplevel#show ();
  midi_window#check_widgets ();
  let iact_window = new iact_window () in
  if show then iact_window#toplevel#show ();
  iact_window#check_widgets ();
  let app_window = new app_window () in
  if show then app_window#toplevel#show ();
  app_window#check_widgets ();
  if show then GMain.Main.main ()
;;
