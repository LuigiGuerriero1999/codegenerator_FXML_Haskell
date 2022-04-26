package com.example.testboy.structures;

import java.util.Objects;

public class RadioButton extends GTKWidget{
    private String text;

    public RadioButton(String text) {
        this.text = text;
    }

    public RadioButton(String id, Integer id_hash, String name, double layoutX, double layoutY, String text) {
        super(id, id_hash, makeName(id, id_hash, name), layoutX, layoutY);
        this.text = text;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public String gtkHsCode(){
        String radioButtonGtkHsCode = super.getName() + " <- Gtk.radioButtonNewFromWidget (Nothing::Maybe Gtk.RadioButton)\n  ";
        if (!Objects.equals(text, "")) radioButtonGtkHsCode = super.getName() + " <- Gtk.radioButtonNewWithLabelFromWidget (Nothing::Maybe Gtk.RadioButton) "+"\""+text+"\""+"\n  ";
        return radioButtonGtkHsCode + "\n  ";
    }
}
