package com.example.testboy.structures;

import com.example.testboy.StringFormat;
import javafx.beans.Observable;
import javafx.collections.ObservableList;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ComboBoxText extends GTKWidget{
    private String comboBoxTextName;

    private boolean entry;
    private ObservableList<?> items;

    private double width;
    private double height;

    public ComboBoxText(String id, Integer id_hash, String name, double layoutX, double layoutY, boolean entry, ObservableList<?> items, double width, double height) {
        super(id, id_hash, makeName(id, id_hash, name+"Container"), layoutX, layoutY);
        this.entry = entry;
        this.items = items;
        this.width = width;
        this.height = height;
        this.comboBoxTextName = makeName(id, id_hash, name);
    }

    public String getComboBoxTextName() {
        return comboBoxTextName;
    }

    public void setComboBoxTextName(String comboBoxTextName) {
        this.comboBoxTextName = comboBoxTextName;
    }

    public boolean isEntry() {
        return entry;
    }

    public void setEntry(boolean entry) {
        this.entry = entry;
    }

    public ObservableList<?> getItems() {
        return items;
    }

    public void setItems(ObservableList<?> items) {
        this.items = items;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }

    @Override
    public String gtkHsCode(){
        String comboBoxTextTemplate;
        if(entry) comboBoxTextTemplate = "${COMBOBOXTEXTNAME} <- Gtk.comboBoxTextNew \n  ";
        else comboBoxTextTemplate = "${COMBOBOXTEXTNAME} <- Gtk.comboBoxTextNewWithEntry \n  ";

        StringBuilder comboBoxTextItems = new StringBuilder("");
        for (Object obj : items){
            String item = obj.toString();
            comboBoxTextItems.append("Gtk.comboBoxTextAppendText ").append(comboBoxTextName).append(" ").append("\"").append(item).append("\"").append("\n  ");
        }

        StringBuilder template = new StringBuilder();
        template.append(comboBoxTextTemplate);
        template.append(comboBoxTextItems);
        template.append("${COMBOBOXCONTAINERNAME} <- Gtk.boxNew OrientationHorizontal 1\n  ");
        template.append("Gtk.set ${COMBOBOXCONTAINERNAME} [Gtk.widgetWidthRequest :=${WIDTH},  Gtk.widgetHeightRequest :=${HEIGHT}]\n  ");
        template.append("Gtk.boxPackStart ${COMBOBOXCONTAINERNAME} ${COMBOBOXTEXTNAME} True True 0\n   ");

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("COMBOBOXCONTAINERNAME", super.getName());
        toInsert.put("WIDTH", (int)width);
        toInsert.put("HEIGHT", (int)height);
        toInsert.put("COMBOBOXTEXTNAME", comboBoxTextName);

        return StringFormat.format(template.toString(),toInsert);
    }
}
