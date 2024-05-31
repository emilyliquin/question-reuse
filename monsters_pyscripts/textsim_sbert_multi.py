from sentence_transformers import SentenceTransformer, util
model = SentenceTransformer('all-MiniLM-L6-v2')

def get_text_sim_multi(question1, questions2):
    # if quesiton 2 is a string, make it a list
    if isinstance(questions2, str):
        questions2 = [questions2]

    #Sentences are encoded by calling model.encode()
    emb1 = model.encode(question1) # this is always a single question
    emb2 = model.encode(questions2) # this can be multiple questions
    cos_sim = util.cos_sim(emb1, emb2)

    output = {}
    for idx_j, question2 in enumerate(questions2):
        output[question2] = cos_sim[0][idx_j].item()

    return output
