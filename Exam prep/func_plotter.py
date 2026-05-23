import tkinter as tk
from tkinter import messagebox
import customtkinter as ctk
import numpy as np
import sympy as sp
from sympy.parsing.latex import parse_latex

# Configurazione per integrare Matplotlib in Tkinter
import matplotlib
matplotlib.use("TkAgg")
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure

# Impostiamo il tema della GUI
ctk.set_appearance_mode("System")  # "System", "Dark", "Light"
ctk.set_default_color_theme("blue")

class LatexPlotterApp(ctk.CTk):
    def __init__(self):
        super().__init__()

        self.title("LaTeX Equation Plotter")
        self.geometry("1000x650")

        # Variabili di stato
        self.sympy_expr = None
        self.free_symbols = []
        self.param_sliders = {}
        self.param_labels = {}

        # Layout Principale (due colonne: sinistra controlli, destra grafico)
        self.grid_columnconfigure(0, weight=1, minsize=350)
        self.grid_columnconfigure(1, weight=2)
        self.grid_rowconfigure(0, weight=1)

        # --- PANNELLO SINISTRO (CONTROLLI) ---
        self.sidebar = ctk.CTkFrame(self, corner_radius=0)
        self.sidebar.grid(row=0, column=0, sticky="nsew", padx=10, pady=10)
        
        # Input Equazione
        self.lbl_eq = ctk.CTkLabel(self.sidebar, text="Inserisci Equazione (LaTeX):", font=ctk.CTkFont(weight="bold"))
        self.lbl_eq.pack(padx=20, pady=(20, 5), anchor="w")
        
        # Esempio preimpostato: a * x^2 + b * sin(c * x)
        self.entry_latex = ctk.CTkEntry(self.sidebar, placeholder_text="es. a * x^2 + b * sin(x)", width=300)
        self.entry_latex.insert(0, r"a \cdot x^2 + b \cdot \sin(c \cdot x)")
        self.entry_latex.pack(padx=20, pady=5, fill="x")

        # Scelta della variabile indipendente
        self.lbl_var = ctk.CTkLabel(self.sidebar, text="Variabile asse X:", font=ctk.CTkFont(weight="bold"))
        self.lbl_var.pack(padx=20, pady=(10, 5), anchor="w")
        self.entry_var = ctk.CTkEntry(self.sidebar, width=100)
        self.entry_var.insert(0, "x")
        self.entry_var.pack(padx=20, pady=5, anchor="w")

        # Bottone per elaborare l'equazione
        self.btn_parse = ctk.CTkButton(self.sidebar, text="Elabora Parametri", command=self.parse_equation)
        self.btn_parse.pack(padx=20, pady=15, fill="x")

        # Separatore visivo
        self.separator = ctk.CTkLabel(self.sidebar, text="--- PARAMETRI ---", font=ctk.CTkFont(size=12, weight="bold"))
        self.separator.pack(padx=20, pady=5)

        # Frame dinamico che conterrà gli slider dei parametri
        self.params_frame = ctk.CTkScrollableFrame(self.sidebar, label_text="Regola i parametri")
        self.params_frame.pack(padx=20, pady=10, fill="both", expand=True)

        # --- PANNELLO DESTRO (GRAFICO) ---
        self.plot_frame = ctk.CTkFrame(self)
        self.plot_frame.grid(row=0, column=1, sticky="nsew", padx=10, pady=10)
        
        # Inizializzazione della figura Matplotlib
        self.fig = Figure(figsize=(6, 5), dpi=100)
        self.ax = self.fig.add_subplot(111)
        self.ax.grid(True, linestyle="--", alpha=0.6)
        
        # Canvas per inserire Matplotlib in CustomTkinter
        self.canvas = FigureCanvasTkAgg(self.fig, master=self.plot_frame)
        self.canvas.get_tk_widget().pack(fill="both", expand=True, padx=10, pady=10)

        # Genera i parametri per l'equazione iniziale di default
        self.parse_equation()

    def parse_equation(self):
        """Prende la stringa LaTeX, la converte con SymPy e crea gli slider per i parametri."""
        latex_str = self.entry_latex.get().strip()
        x_var_str = self.entry_var.get().strip()

        if not latex_str or not x_var_str:
            messagebox.showerror("Errore", "Inserisci sia l'equazione che la variabile asse X.")
            return

        try:
            # Converte il LaTeX in un'espressione SymPy
            self.sympy_expr = parse_latex(latex_str)
            
            # Trova tutte le lettere/simboli nell'equazione
            all_symbols = self.sympy_expr.free_symbols
            all_symbols_names = {s.name for s in all_symbols}

            if x_var_str not in all_symbols_names:
                # Se la x non è espressa, la aggiungiamo comunque ai simboli per sicurezza
                self.x_symbol = sp.Symbol(x_var_str)
            else:
                self.x_symbol = sp.Symbol(x_var_str)

            # I parametri sono tutti i simboli tranne la variabile indipendente (la X)
            self.free_symbols = sorted([s for s in all_symbols if s.name != x_var_str], key=lambda s: s.name)

            # Puliamo i vecchi slider se presenti
            for widget in self.params_frame.winfo_children():
                widget.destroy()
            
            self.param_sliders = {}
            self.param_labels = {}

            # Creiamo dinamicamente uno slider per ogni parametro trovato
            for sym in self.free_symbols:
                name = sym.name
                
                # Frame contenitore per ogni singolo parametro
                row_frame = ctk.CTkFrame(self.params_frame, fg_color="transparent")
                row_frame.pack(fill="x", pady=5)

                # Etichetta col nome e valore corrente
                lbl = ctk.CTkLabel(row_frame, text=f"{name}: 1.00", width=80, anchor="w")
                lbl.pack(side="left", padx=5)
                self.param_labels[name] = lbl

                # Slider (da -10 a 10, valore iniziale 1.0)
                slider = ctk.CTkSlider(
                    row_frame, 
                    from_=-10.0, 
                    to=10.0, 
                    number_of_steps=200,
                    command=lambda val, n=name: self.on_slider_move(n, val)
                )
                slider.set(1.0)
                slider.pack(side="right", fill="x", expand=True, padx=5)
                self.param_sliders[name] = slider

            # Disegna il grafico per la prima volta
            self.update_plot()

        except Exception as e:
            messagebox.showerror("Errore di Parsing", f"Impossibile leggere l'equazione LaTeX.\nSpecifiche: {e}")

    def on_slider_move(self, name, value):
        """Aggiorna il testo del parametro e ridisegna il grafico al movimento dello slider."""
        self.param_labels[name].configure(text=f"{name}: {float(value):.2f}")
        self.update_plot()

    def update_plot(self):
        """Calcola i punti dell'equazione e aggiorna il grafico Matplotlib."""
        if self.sympy_expr is None:
            return

        # Genera i punti per l'asse X
        x_vals = np.linspace(-10, 10, 500)

        # Crea un dizionario con i valori correnti di tutti i parametri regolati dagli slider
        subs_dict = {}
        for sym in self.free_symbols:
            subs_dict[sym] = self.param_sliders[sym.name].get()

        # Sostituisce i parametri nell'equazione principale
        expr_with_params = self.sympy_expr.subs(subs_dict)

        # Trasforma l'espressione di SymPy in una funzione NumPy ultra-veloce da calcolare
        try:
            # lambdify accetta la variabile indipendente e l'espressione rimasta
            func = sp.lambdify(self.x_symbol, expr_with_params, modules=['numpy', 'math'])
            y_vals = func(x_vals)
            
            # Gestione nel caso in cui la funzione restituisca un valore costante anziché un array
            if isinstance(y_vals, (int, float)):
                y_vals = np.full_like(x_vals, y_vals)

            # Aggiorna il plot di Matplotlib
            self.ax.clear()
            self.ax.grid(True, linestyle="--", alpha=0.6)
            
            # Plot della linea principale
            self.ax.plot(x_vals, y_vals, label=f"$y = {sp.latex(self.sympy_expr)}$", color="#1f77b4", linewidth=2)
            
            # Impostazioni estetiche del grafico
            self.ax.set_title("Grafico della Funzione", fontsize=12, pad=10)
            self.ax.set_xlabel(self.x_symbol.name)
            self.ax.set_ylabel("y")
            self.ax.axhline(0, color='black', linewidth=0.5, alpha=0.5)
            self.ax.axvline(0, color='black', linewidth=0.5, alpha=0.5)
            
            # Mostra la legenda formattata in LaTeX
            self.ax.legend(loc="upper right")

            # Applica i cambiamenti al canvas della GUI
            self.canvas.draw()

        except Exception as e:
            # Evitiamo continui pop-up di errore durante il movimento fluido dello slider
            pass

if __name__ == "__main__":
    app = LatexPlotterApp()
    app.mainloop()