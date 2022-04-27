package com.example.testboy.togglegroup;

import com.example.testboy.structures.GTKWidget;

import java.util.ArrayList;

public class ToggleGroup {
    private int hashcode;
    private ArrayList<GTKWidget> buttons;

    public ToggleGroup(int hashcode, ArrayList<GTKWidget> buttons) {
        this.hashcode = hashcode;
        this.buttons = buttons;
    }

    public int getHashcode() {
        return hashcode;
    }

    public void setHashcode(int hashcode) {
        this.hashcode = hashcode;
    }

    public ArrayList<GTKWidget> getButtons() {
        return buttons;
    }

    public void setButtons(ArrayList<GTKWidget> buttons) {
        this.buttons = buttons;
    }

    public ToggleGroup getToggleGroup(ArrayList<ToggleGroup> toggleGroups){
        if (!toggleGroups.isEmpty()){
            for (ToggleGroup tg : toggleGroups) {
                if (tg.hashcode == this.hashcode) {
                    return tg;
                }
            }
        }
        return this;
    }

    public void addButtonToToggleGroup(GTKWidget button){
        buttons.add(button);
    }

    public String gtkHsCode(){
        String firstButton = buttons.get(0).getName();
        ArrayList<GTKWidget> newList = buttons;
        newList.remove(0);

        StringBuilder GtkHsCode = new StringBuilder("");
        if (newList.isEmpty()){
            // do nothing
        } else {
            GtkHsCode.append("Gtk.radioButtonSetGroup ").append(firstButton).append(" ").append("[");
            for (GTKWidget widget : newList){
                String widgetName = widget.getName();
                if (newList.get(newList.size()-1) == widget){
                    GtkHsCode.append(widgetName);
                } else {
                    GtkHsCode.append(widgetName).append(", ");
                }

            }
            GtkHsCode.append("]\n  ");
        }

        return GtkHsCode.toString();
    }
}
