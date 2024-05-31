## script to get EIGs from a dataframe of questions
from eig.monsters import ObjectSetHypothesisSpace, UniformDistribution
import numpy as np
import pandas as pd
from eig import compute_eig, Bayes, Context, entropy
from eig.monsters import Parser, Executor


def add_eigs(x):
    df = pd.DataFrame(x)
    df["EIG"] = 0.0
    df["entropy"] = 0.0
    for index, row in df.iterrows():
        hypotheses = ObjectSetHypothesisSpace(object_colors=["red", "blue", "purple"], object_sizes=[1, 2, 3], object_shapes=[1, 2])
        prior = UniformDistribution()  
        belief = Bayes(hypotheses, prior)   

        q = row["question_program"]
        partial_info = np.array([[row["r_shape"], row["r_legs"]], [row["b_shape"], row["b_legs"]], [row["p_shape"], row["p_legs"]]])

        context = Context(hypotheses, belief)   # context stores the posterior belief
        context.observe(partial_info)
        ent = entropy(context.belief.belief)
        df.loc[index,"entropy"] = ent

        try:
            question = Parser.parse(q)
            executor = Executor(question)
            eig = compute_eig(executor, context)
            df.loc[index,"EIG"] = eig

        except:
            df.loc[index,"EIG"] = -999

    return df